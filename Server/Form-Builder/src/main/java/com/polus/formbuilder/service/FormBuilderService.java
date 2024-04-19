package com.polus.formbuilder.service;

import java.util.List;

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
import com.polus.formbuilder.model.FormEvaluateValidationResponse;
import com.polus.formbuilder.model.FormRequest;
import com.polus.formbuilder.model.FormResponse;
import com.polus.formbuilder.model.FormValidationRequest;

@Service
public interface FormBuilderService {
	
	public ApplicableFormResponse getApplicableForms(ApplicableFormRequest request);

	public BlankFormResponse GetBankFormbyModule(BlankFormRequest request);
	
	public BlankFormResponse getBlankFormByFormId(BlankFormRequest request);
		
	public FormResponse getFormbyFormId(FormRequest request);
	
	public FormResponse GetFormSection(FormRequest request);
	
	
	//Get Component Services
	public FormComponentFetchResponse GetQuestionnaireComponent(FormComponentFetchRequest request); 
	
	public FormComponentFetchResponse GetCustomElementComponent(FormComponentFetchRequest request);
	
	public FormComponentFetchResponse GetProgrammedElementComponent(FormComponentFetchRequest request);
	
	
	//Save Component Services
	public FormComponentSaveResponse SaveQuestionnaireComponent(FormComponentSaveRequest request, 
																MultipartHttpServletRequest multipartRequest);
	
	public FormComponentSaveResponse SaveCustomElementComponent(FormComponentSaveRequest request);
	
	public FormComponentSaveResponse SaveProgrammedElementComponent(FormComponentSaveRequest request);

	// Validation services
	public List<FormEvaluateValidationResponse> validateForm(FormValidationRequest formValidationRequest);

	public List<FormEvaluateValidationResponse> validateSection(FormValidationRequest formValidationRequest);

	public List<FormEvaluateValidationResponse> validateComponent(FormValidationRequest formValidationRequest);

}
