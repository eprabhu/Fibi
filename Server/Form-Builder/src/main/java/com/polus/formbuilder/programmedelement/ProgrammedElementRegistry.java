package com.polus.formbuilder.programmedelement;

import java.util.HashMap;

import com.polus.formbuilder.programmedelement.opa.compuncomp.OPACompUnComp;

public class ProgrammedElementRegistry {
	
	
	private static HashMap<String, Class<? extends ProgrammedElement>> registry = new HashMap<>();
	
	private ProgrammedElementRegistry() {
		registry.put(ProgrammedElementConstants.OPA_COMP_UNCOMP, OPACompUnComp.class);
	}
	
	public Class<? extends ProgrammedElement> getElementClass(String type) {
        return registry.get(type);
    }
	
	private static class ProgrammedElementRegistryHelper{
		private static final ProgrammedElementRegistry INSTANCE = new ProgrammedElementRegistry();
	}
	
	public static ProgrammedElementRegistry instance() {
		return ProgrammedElementRegistryHelper.INSTANCE;
	}
}
