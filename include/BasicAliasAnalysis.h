#ifndef _H_SONGLH_BASICALIASANALYSIS
#define _H_SONGLH_BASICALIASANALYSIS


#include "AliasAnalysis.h"


#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/CaptureTracking.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Operator.h"
#include "llvm/Pass.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/GetElementPtrTypeIterator.h"
#include "llvm/Target/TargetLibraryInfo.h"


enum ExtensionKind {
    EK_NotExtended,
    EK_SignExt,
    EK_ZeroExt
};

struct VariableGEPIndex {
    const llvm::Value *V;
    ExtensionKind Extension;
    int64_t Scale;

    bool operator==(const VariableGEPIndex &Other) const {
      return V == Other.V && Extension == Other.Extension &&
        Scale == Other.Scale;
    }

    bool operator!=(const VariableGEPIndex &Other) const {
      return !operator==(Other);
    }
};


class BasicAliasAnalysis : public llvm::ImmutablePass, public AliasAnalysis
{
public:
	static char ID;
	llvm::Function * pCurrentFunction;
	llvm::LoopInfo * pLI;
	llvm::DominatorTree * pDT;


	BasicAliasAnalysis(): ImmutablePass(ID) {}


	

	virtual void initializePass() {}

	virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const {}

	virtual AliasResult alias(const Location &LocA,
                              const Location &LocB);


    virtual ModRefResult getModRefInfo(llvm::ImmutableCallSite CS,
                                       const Location &Loc);


    virtual ModRefResult getModRefInfo(llvm::ImmutableCallSite CS1,
                                       llvm::ImmutableCallSite CS2) {
      // The AliasAnalysis base class has some smarts, lets use them.
      return AliasAnalysis::getModRefInfo(CS1, CS2);
    }



    /// pointsToConstantMemory - Chase pointers until we find a (constant
    /// global) or not.
    virtual bool pointsToConstantMemory(const Location &Loc, bool OrLocal);

    /// getModRefBehavior - Return the behavior when calling the given
    /// call site.
    virtual ModRefBehavior getModRefBehavior(llvm::ImmutableCallSite CS);

    /// getModRefBehavior - Return the behavior when calling the given function.
    /// For use when the call site is not known.
    virtual ModRefBehavior getModRefBehavior(const llvm::Function *F);

    /// getAdjustedAnalysisPointer - This method is used when a pass implements
    /// an analysis interface through multiple inheritance.  If needed, it
    /// should override this to adjust the this pointer as needed for the
    /// specified pass info.
    /*
    virtual void *getAdjustedAnalysisPointer(const void *ID) {
      if (ID == &AliasAnalysis::ID)
        return (AliasAnalysis*)this;
      return this;
    }*/

  private:
    // AliasCache - Track alias queries to guard against recursion.
    typedef std::pair<Location, Location> LocPair;
    typedef llvm::SmallDenseMap<LocPair, AliasResult, 8> AliasCacheTy;
    AliasCacheTy AliasCache;

    /// \brief Track phi nodes we have visited. When interpret "Value" pointer
    /// equality as value equality we need to make sure that the "Value" is not
    /// part of a cycle. Otherwise, two uses could come from different
    /// "iterations" of a cycle and see different values for the same "Value"
    /// pointer.
    /// The following example shows the problem:
    ///   %p = phi(%alloca1, %addr2)
    ///   %l = load %ptr
    ///   %addr1 = gep, %alloca2, 0, %l
    ///   %addr2 = gep  %alloca2, 0, (%l + 1)
    ///      alias(%p, %addr1) -> MayAlias !
    ///   store %l, ...
    llvm::SmallPtrSet<const llvm::BasicBlock*, 8> VisitedPhiBBs;

    // Visited - Track instructions visited by pointsToConstantMemory.
    llvm::SmallPtrSet<const llvm::Value*, 16> Visited;


    /// \brief Check whether two Values can be considered equivalent.
    ///
    /// In addition to pointer equivalence of \p V1 and \p V2 this checks
    /// whether they can not be part of a cycle in the value graph by looking at
    /// all visited phi nodes an making sure that the phis cannot reach the
    /// value. We have to do this because we are looking through phi nodes (That
    /// is we say noalias(V, phi(VA, VB)) if noalias(V, VA) and noalias(V, VB).
    bool isValueEqualInPotentialCycles(const llvm::Value *V1, const llvm::Value *V2);


    /// \brief Dest and Src are the variable indices from two decomposed
    /// GetElementPtr instructions GEP1 and GEP2 which have common base
    /// pointers.  Subtract the GEP2 indices from GEP1 to find the symbolic
    /// difference between the two pointers.
    void GetIndexDifference(llvm::SmallVectorImpl<VariableGEPIndex> &Dest,
                            const llvm::SmallVectorImpl<VariableGEPIndex> &Src);


    // aliasGEP - Provide a bunch of ad-hoc rules to disambiguate a GEP
    // instruction against another.
    AliasResult aliasGEP(const llvm::GEPOperator *V1, uint64_t V1Size,
                         const llvm::MDNode *V1TBAAInfo,
                         const llvm::Value *V2, uint64_t V2Size,
                         const llvm::MDNode *V2TBAAInfo,
                         const llvm::Value *UnderlyingV1, const llvm::Value *UnderlyingV2);


    // aliasPHI - Provide a bunch of ad-hoc rules to disambiguate a PHI
    // instruction against another.
    AliasResult aliasPHI(const llvm::PHINode *PN, uint64_t PNSize,
                         const llvm::MDNode *PNTBAAInfo,
                         const llvm::Value *V2, uint64_t V2Size,
                         const llvm::MDNode *V2TBAAInfo);

    /// aliasSelect - Disambiguate a Select instruction against another value.
    AliasResult aliasSelect(const llvm::SelectInst *SI, uint64_t SISize,
                            const llvm::MDNode *SITBAAInfo,
                            const llvm::Value *V2, uint64_t V2Size,
                            const llvm::MDNode *V2TBAAInfo);

    AliasResult aliasCheck(const llvm::Value *V1, uint64_t V1Size,
                           const llvm::MDNode *V1TBAATag,
                           const llvm::Value *V2, uint64_t V2Size,
                           const llvm::MDNode *V2TBAATag);

                          
};


#endif

