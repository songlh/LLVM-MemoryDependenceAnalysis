#ifndef _H_SONGLH_NOALIASANALYSIS
#define _H_SONGLH_NOALIASANALYSIS

#include "AliasAnalysis.h"
#include "NoAliasAnalysis.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Pass.h"


class NoAA : public AliasAnalysis
{
public:
	static char ID;
	NoAA() : AliasAnalysis()
	{}
	
	virtual AliasResult alias(const Location &LocA, const Location &LocB) {
      return MayAlias;
    }

    virtual ModRefBehavior getModRefBehavior(llvm::ImmutableCallSite CS) {
      return UnknownModRefBehavior;
    }
    virtual ModRefBehavior getModRefBehavior(const llvm::Function *F) {
      return UnknownModRefBehavior;
    }

    virtual bool pointsToConstantMemory(const Location &Loc,
                                        bool OrLocal) {
      return false;
    }
    virtual ModRefResult getModRefInfo(llvm::ImmutableCallSite CS,
                                       const Location &Loc) {
      return ModRef;
    }
    virtual ModRefResult getModRefInfo(llvm::ImmutableCallSite CS1,
                                       llvm::ImmutableCallSite CS2) {
      return ModRef;
    }

    virtual void deleteValue(llvm::Value *V) {}
    virtual void copyValue(llvm::Value *From, llvm::Value *To) {}
    virtual void addEscapingUse(llvm::Use &U) {}
    
    /// getAdjustedAnalysisPointer - This method is used when a pass implements
    /// an analysis interface through multiple inheritance.  If needed, it
    /// should override this to adjust the this pointer as needed for the
    /// specified pass info.
    /*
    virtual void *getAdjustedAnalysisPointer(const void *ID) {
      if (ID == &AliasAnalysis::ID)
        return (AliasAnalysis*)this;
      return this;
    }
    */

};



#endif

