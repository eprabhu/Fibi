package com.polus.formbuilder.service;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.polus.formbuilder.model.ApplicableFormRequest;
import com.polus.formbuilder.model.ApplicableFormResponse;
import com.polus.formbuilder.model.BlankFormRequest;
import com.polus.formbuilder.model.BlankFormResponse;
import com.polus.formbuilder.model.FormComponentFetchRequest;
import com.polus.formbuilder.model.FormComponentFetchResponse;
import com.polus.formbuilder.model.FormComponentSaveRequest;
import com.polus.formbuilder.model.FormComponentSaveResponse;
import com.polus.formbuilder.model.FormRequest;
import com.polus.formbuilder.model.FormResponse;
import com.polus.formbuilder.model.FormValidationRequest;

@Service
public class FormBuilderServiceCoordinator {
	
	
	@Autowired
	private Map<String, FormBuilderService> formBuilderServiceImp;
	
		
	public ApplicableFormResponse getApplicableForms(ApplicableFormRequest request) {
		FormBuilderService formBuilderService = getImplementationClass(request.getModuleItemCode());
		return formBuilderService.getApplicableForms(request);
	}
	
	public BlankFormResponse GetBankForm(BlankFormRequest request) {
		FormBuilderService formBuilderService = getImplementationClass(request.getModuleItemCode());
		if(request.getFormBuilderId() != null){
			return formBuilderService.getBlankFormByFormId(request);			
		}
		
		return formBuilderService.GetBankFormbyModule(request);
	}
	
	public FormResponse GetForm(FormRequest request) {
		FormBuilderService formBuilderService = getImplementationClass(request.getModuleItemCode());
		return formBuilderService.getFormbyFormId(request);
	}
	
	public FormResponse GetFormSection(FormRequest request) {
		FormBuilderService formBuilderService = getImplementationClass(request.getModuleItemCode());		
		return formBuilderService.GetFormSection(request);
	}
	
	public FormComponentFetchResponse getFormComponent(FormComponentFetchRequest request) {
		FormBuilderService formBuilderService = getImplementationClass(request.getModuleItemCode());
		
		if(request.getComponentType().equals(FormBuilderConstants.QUESTIONNAIR_COMPONENT)) {
			return formBuilderService.GetQuestionnaireComponent(request);
				
		}else if(request.getComponentType().equals(FormBuilderConstants.CUSTOM_ELEMENT_COMPONENT)) {
			return formBuilderService.GetCustomElementComponent(request);
			
		}else if(request.getComponentType().equals(FormBuilderConstants.PROGRAMMED_ELEMENT_COMPONENT)) {
			return formBuilderService.GetProgrammedElementComponent(request);
			
		}		
		return null;
				
	}

	public FormComponentSaveResponse saveFormComponent(FormComponentSaveRequest request, 
											 MultipartHttpServletRequest multipartRequest) {
		
		FormBuilderService formBuilderService = getImplementationClass(request.getModuleItemCode());
		
		if(request.getComponentType().equals(FormBuilderConstants.QUESTIONNAIR_COMPONENT)) {
			return formBuilderService.SaveQuestionnaireComponent(request,multipartRequest);
				
		} else if (FormBuilderConstants.CUSTOM_ELEMENT_COMPONENT_LIST.contains(request.getComponentType())) {
			return formBuilderService.SaveCustomElementComponent(request);
			
		} else if (request.getComponentType().equals(FormBuilderConstants.PROGRAMMED_ELEMENT_COMPONENT)) {
			return formBuilderService.SaveProgrammedElementComponent(request);	
		}	
		
		return null;
	}
	
	private FormBuilderService getImplementationClass(String moduleCode) {		
		return formBuilderServiceImp.get(getFormBuilderServiceImpKey(moduleCode));
	}
	
	private String getFormBuilderServiceImpKey(String moduleCode) {
	    switch (moduleCode) {
	        case FormBuilderConstants.OPA_MODULE,
	             FormBuilderConstants.CONSULTING_DISCLOSURE_MODULE:
	            return "Module_".concat(moduleCode).concat("_FormBuilderServiceImpl");
	        default:
	            return "Module_0_FormBuilderServiceImpl";
	    }
	}


	public ResponseEntity<Object> validateForm(FormValidationRequest formValidationRequest) {
		if (formValidationRequest.getFormBuilderIds() == null || formValidationRequest.getFormBuilderIds().isEmpty()) {
			return new ResponseEntity<>("Form Id not present!!", HttpStatus.OK);
		}
		FormBuilderService formBuilderService = getImplementationClass(formValidationRequest.getModuleItemCode());
		return new ResponseEntity<>(formBuilderService.validateForm(formValidationRequest), HttpStatus.OK);
	}

	public ResponseEntity<Object> validateSection(FormValidationRequest formValidationRequest) {
		if (formValidationRequest.getFormBuilderSectionId() == null) {
			return new ResponseEntity<>("Section Id not present!!", HttpStatus.OK);
		}
		FormBuilderService formBuilderService = getImplementationClass(formValidationRequest.getModuleItemCode());
		return new ResponseEntity<>(formBuilderService.validateSection(formValidationRequest), HttpStatus.OK);
	}

	public ResponseEntity<Object> validateComponent(FormValidationRequest formValidationRequest) {
		if (formValidationRequest.getFormBuilderSectCompId() == null) {
			return new ResponseEntity<>("Component Id not present!!", HttpStatus.OK);
		}
		FormBuilderService formBuilderService = getImplementationClass(formValidationRequest.getModuleItemCode());
		return new ResponseEntity<>(formBuilderService.validateComponent(formValidationRequest), HttpStatus.OK);
	}

}
