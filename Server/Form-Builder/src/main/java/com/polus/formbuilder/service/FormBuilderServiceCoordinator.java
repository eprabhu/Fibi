package com.polus.formbuilder.service;

import org.springframework.beans.factory.annotation.Autowired;
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

@Service
public class FormBuilderServiceCoordinator {
	
	@Autowired
	private FormBuilderModuleRouter moduleRouter;
	
	public ApplicableFormResponse getApplicableForms(ApplicableFormRequest request) {
		FormBuilderService formBuilderService = moduleRouter.route(request.getModuleItemCode());
		return formBuilderService.getApplicableForms(request);
	}
	
	public BlankFormResponse GetBankForm(BlankFormRequest request) {
		FormBuilderService formBuilderService = moduleRouter.route(request.getModuleItemCode());
		if(request.getFormBuilderId() != null){
			return formBuilderService.getBlankFormByFormId(request);			
		}
		
		return formBuilderService.GetBankFormbyModule(request);
	}
	
	public FormResponse GetForm(FormRequest request) {
		FormBuilderService formBuilderService = moduleRouter.route(request.getModuleItemCode());
		return formBuilderService.getFormbyFormId(request);
	}
	
	public FormResponse GetFormSection(FormRequest request) {
		FormBuilderService formBuilderService = moduleRouter.route(request.getModuleItemCode());		
		return formBuilderService.GetFormSection(request);
	}
	
	public FormComponentFetchResponse getFormComponent(FormComponentFetchRequest request) {
		FormBuilderService formBuilderService = moduleRouter.route(request.getModuleItemCode());	
		
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
		
		FormBuilderService formBuilderService = moduleRouter.route(request.getModuleItemCode());	
		
		if(request.getComponentType().equals(FormBuilderConstants.QUESTIONNAIR_COMPONENT)) {
			return formBuilderService.SaveQuestionnaireComponent(request,multipartRequest);
				
		}else if(request.getComponentType().equals(FormBuilderConstants.CUSTOM_ELEMENT_COMPONENT)) {
			return formBuilderService.SaveCustomElementComponent(request);
			
		}else if(request.getComponentType().equals(FormBuilderConstants.PROGRAMMED_ELEMENT_COMPONENT)) {
			return formBuilderService.SaveProgrammedElementComponent(request);
			
		}	
		
		return null;
	}

}
