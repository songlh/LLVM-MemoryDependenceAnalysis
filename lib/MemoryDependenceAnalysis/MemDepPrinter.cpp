#include "llvm/Pass.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Assembly/Writer.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"

#include "AliasAnalysis.h"
#include "NoAliasAnalysis.h"
#include "BasicAliasAnalysis.h"
#include "MemoryDependenceAnalysis.h"

using namespace llvm;

struct MemDepPrinter : public ModulePass
{
	Function * pF;

	enum DepType {
		Clobber = 0,
		Def,
		NonFuncLocal,
		Unknown
	};

	static const char *const DepTypeStr[];

	typedef PointerIntPair<const Instruction *, 2, DepType> InstTypePair;
	typedef std::pair<InstTypePair, const BasicBlock *> Dep;
	typedef SmallSetVector<Dep, 4> DepSet;
	typedef DenseMap<const Instruction *, DepSet> DepSetMap;
	DepSetMap Deps;

	static char ID; // Pass identifcation, replacement for typeid
	MemDepPrinter() : ModulePass(ID) {
		PassRegistry &Registry = *PassRegistry::getPassRegistry();
		initializeDataLayoutPass(Registry);
		initializeTargetLibraryInfoPass(Registry);
		initializeDominatorTreePass(Registry);
		initializeLoopInfoPass(Registry);
    }

	virtual bool runOnModule(Module &M);

	void print(raw_ostream &OS, const Module * = 0) const;

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<DataLayout>();
		AU.addRequired<TargetLibraryInfo>();
		AU.addRequired<DominatorTree>();
		AU.addRequired<LoopInfo>();
		AU.setPreservesAll();
    }


	virtual void releaseMemory() {
		Deps.clear();
		pF = 0;
	}


private:
	static InstTypePair getInstTypePair(MemDepResult dep) {
		if (dep.isClobber())
			return InstTypePair(dep.getInst(), Clobber);
		if (dep.isDef())
			return InstTypePair(dep.getInst(), Def);
		if (dep.isNonFuncLocal())
			return InstTypePair(dep.getInst(), NonFuncLocal);
		assert(dep.isUnknown() && "unexptected dependence type");
		return InstTypePair(dep.getInst(), Unknown);
	}

	static InstTypePair getInstTypePair(const Instruction* inst, DepType type) {
		return InstTypePair(inst, type);
	}
	
};


char MemDepPrinter::ID = 0;

RegisterPass<MemDepPrinter> X("PrintMemDepDebug", "Print Dep Value", true, false);

const char *const MemDepPrinter::DepTypeStr[]
  = {"Clobber", "Def", "NonFuncLocal", "Unknown"};



 bool MemDepPrinter::runOnModule(Module & M)
 {
 	Function * F = M.getFunction("main");
 	this->pF = F;
 	
 	DataLayout * pDL = &(getAnalysis<DataLayout>());
 	TargetLibraryInfo * pTLI = &(getAnalysis<TargetLibraryInfo>());

 	LoopInfo * pLoop = &(getAnalysis<LoopInfo>(*F));
 	DominatorTree * pDT = &(getAnalysis<DominatorTree>(*F));


 	NoAA * pNoAA = new NoAA();
 	BasicAliasAnalysis * pBAA = new BasicAliasAnalysis();
 	pNoAA->TD = pDL;
 	pNoAA->TLI = pTLI;
 	pBAA->TD = pDL;
 	pBAA->TLI = pTLI;
 	pBAA->pLI = pLoop;
 	pBAA->pDT = pDT;
 	pBAA->AA = pNoAA;

 	AliasAnalysis * AA = pBAA;

 	MemoryDependenceAnalysis * pMemDep = new MemoryDependenceAnalysis();
 	pMemDep->AA = pBAA;
 	pMemDep->TD = pDL;
 	pMemDep->DT = pDT;
 	pMemDep->runOnFunction(*F);




 	for(Function::iterator BB = F->begin(), BE = F->end(); BB != BE; BB++)
 	{
 		for(BasicBlock::iterator I = BB->begin(), IE = BB->end(); I != IE; I ++)
 		{
 			if(!I->mayReadFromMemory() && !I->mayWriteToMemory() )
 			{
 				continue;
 			}

 			//I->dump();
 			MemDepResult Res = pMemDep->getDependency(I);
 			if(!Res.isNonLocal())
 			{
 				Deps[I].insert(std::make_pair(getInstTypePair(Res), static_cast<BasicBlock *>(0)));
 			}
 			else if(CallSite CS = cast<Value>(I))
 			{
 				/*
 				const MemoryDependenceAnalysis::NonLocalDepInfo &NLDI =
 					pMemDep->getNonLocalCallDependency(CS);
 				DepSet &InstDeps = Deps[I];

 				for (MemoryDependenceAnalysis::NonLocalDepInfo::const_iterator
					MI = NLDI.begin(), ME = NLDI.end(); MI != ME; ++MI) {
					const MemDepResult &Res = MI->getResult();
					InstDeps.insert(std::make_pair(getInstTypePair(Res), MI->getBB()));
				}
				*/


 			}
 			else
 			{
 				SmallVector<NonLocalDepResult, 4> NLDI;
 				if(LoadInst *LI = dyn_cast<LoadInst>(I))
 				{
 					if(!LI->isUnordered())
 					{
 						Deps[I].insert(std::make_pair(getInstTypePair(0, Unknown), static_cast<BasicBlock *>(0)));
 						continue;
 					}

 					AliasAnalysis::Location Loc = AA->getLocation(LI);
 					pMemDep->getNonLocalPointerDependency(Loc, true, LI->getParent(), NLDI);
 				}
 				else if(StoreInst * SI = dyn_cast<StoreInst>(I) )
 				{
 					if(!SI->isUnordered())
 					{
 						Deps[I].insert(std::make_pair(getInstTypePair(0, Unknown), static_cast<BasicBlock *>(0)) );
 						continue;
 					}

 					AliasAnalysis::Location Loc = AA->getLocation(SI);
 					pMemDep->getNonLocalPointerDependency(Loc, false, SI->getParent(), NLDI);
 				}
 				else if(VAArgInst * VI = dyn_cast<VAArgInst>(I) )
 				{
 					AliasAnalysis::Location Loc = AA->getLocation(VI);
 					pMemDep->getNonLocalPointerDependency(Loc, false, VI->getParent(), NLDI);
 				}
 				else
 				{

 					llvm_unreachable("Unknown memory instruction!");
 				}



 				DepSet & InstDeps = Deps[I];

 				for(SmallVectorImpl<NonLocalDepResult>::const_iterator itBegin = NLDI.begin(), itEnd = NLDI.end(); 
 						itBegin != itEnd; itBegin++ )
 				{
 					const MemDepResult & Res = itBegin->getResult();
 					InstDeps.insert(std::make_pair(getInstTypePair(Res), itBegin->getBB()));
 				}
 			}
 		}
 	}

 	print(errs(), &M);

 	delete pNoAA;
 	delete pBAA;
 	delete pMemDep;
 	return false;
 }




void MemDepPrinter::print(raw_ostream &OS, const Module *M) const {


	for(Function::iterator BB = this->pF->begin(), BE = this->pF->end(); BB != BE; BB ++ )
	{
		for (BasicBlock::iterator II = BB->begin(), IE = BB->end(); II != IE; ++II) 
		{
			const Instruction *Inst = &*II;

			DepSetMap::const_iterator DI = Deps.find(Inst);
			if (DI == Deps.end())
				continue;

			const DepSet &InstDeps = DI->second;

			for (DepSet::const_iterator I = InstDeps.begin(), E = InstDeps.end(); I != E; ++I) 
			{
				const Instruction *DepInst = I->first.getPointer();
				DepType type = I->first.getInt();
				const BasicBlock *DepBB = I->second;

				OS << "    ";
				OS << DepTypeStr[type];
				if (DepBB) {
					OS << " in block ";
					WriteAsOperand(OS, DepBB, false, M);
				}
				if (DepInst) {
					OS << " from: ";
					DepInst->print(OS);
				}
				OS << "\n";
			}

			Inst->print(OS);
			OS << "\n\n";
		}
	}
	
}



