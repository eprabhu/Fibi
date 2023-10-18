package com.polus.formbuilder.programmedelement;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.formbuilder.programmedelement.opa.compuncomp.OPACompUnComp;
import com.polus.formbuilder.service.module.opa.OPAFormBuilderService;

@Service
public class ProgrammedElementService {

	@Autowired
	private Map<String, ProgrammedElement> programmedElements;

	public ProgrammedElementModel getBlankResponse(String elementName) {
		ProgrammedElement object = programmedElements.get(elementName);
		if (object == null) {
			throw new RuntimeException("Element not found for name: " + elementName);
		}
		return object.getBlankResponse();
	}

	public ProgrammedElementModel getResponse(String elementName, ProgrammedElementModuleDetails moduleDetails,
			ProgrammedElementModel request) {
		
		ProgrammedElement object = programmedElements.get(elementName);
		if (object == null) {
			throw new RuntimeException("Element not found for name: " + elementName);
		}
		return object.getResponse(moduleDetails, request);
	}

	public ProgrammedElementModel performAction(String elementName, ProgrammedElementModuleDetails moduleDetails,
			ProgrammedElementModel request) {

		ProgrammedElement object = programmedElements.get(elementName);
		if (object == null) {
			throw new RuntimeException("Element not found for name: " + elementName);
		}

		return object.save(moduleDetails, request);

	}
}
