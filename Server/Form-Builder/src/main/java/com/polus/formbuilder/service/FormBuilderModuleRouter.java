package com.polus.formbuilder.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.formbuilder.service.module.opa.OPAFormBuilderService;

@Service
public class FormBuilderModuleRouter {

	@Autowired
	private OPAFormBuilderService opaBuilderService;
	
	@Autowired
	private FormBuilderServiceDefaultImp defaultService;
	
			
	public FormBuilderService route(String moduleCode) {
		switch (moduleCode) {
		case FormBuilderConstants.OPA_MODULE:
			return opaBuilderService;

		default:
			return defaultService;
		}
	}
}
