package com.polus.formbuilder.programmedelement;

public class ProgrammedElementFactory {
	
	private static final ProgrammedElementRegistry programmedElementRegistry = ProgrammedElementRegistry.instance();

    // Factory method to create elementClass based on type
    public static ProgrammedElement createProgrammedElement(String type) {
        Class<? extends ProgrammedElement> elementClass = programmedElementRegistry.getElementClass(type);
        if (elementClass == null) {
            throw new IllegalArgumentException("Invalid Programmed Element type: " + type);
        }

        try {
            return elementClass.getDeclaredConstructor().newInstance();
            
        } catch (Exception e) {
            throw new RuntimeException("Error creating Programmed Element", e);
        }
    }
}
