package com.polus.formbuilder.service.module.consultingdisclosure;

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
import com.polus.formbuilder.service.FormBuilderService;
import com.polus.formbuilder.service.FormBuilderServiceProcessor;

@Service("Module_27_FormBuilderServiceImpl")
public class ConsultingDisclFormBuilderService implements FormBuilderService {

    private final FormBuilderServiceProcessor formBuilderProcessor;

    public ConsultingDisclFormBuilderService(FormBuilderServiceProcessor formBuilderProcessor) {
        this.formBuilderProcessor = formBuilderProcessor;
    }
    
    @Override
	public ApplicableFormResponse getApplicableForms(ApplicableFormRequest request) {
    	return formBuilderProcessor.PerformGetApplicableForms(request);
	}
    
	@Override
	public BlankFormResponse GetBankFormbyModule(BlankFormRequest request) {	
		return formBuilderProcessor.PerformGetBankFormbyModule(request);
	}

	@Override
	public BlankFormResponse getBlankFormByFormId(BlankFormRequest request) {
		return formBuilderProcessor.PerformGetBankFormbyFormId(request);
	}

	@Override
	public FormResponse getFormbyFormId(FormRequest request) {
		return formBuilderProcessor.PerformGetForm(request);
	}

	@Override
	public FormResponse GetFormSection(FormRequest request) {
		return formBuilderProcessor.PerformGetFormSection(request);
	}

	@Override
	public FormComponentFetchResponse GetQuestionnaireComponent(FormComponentFetchRequest request) {
		return formBuilderProcessor.PerformGetQuestionnaireComponent(request);
	}

	@Override
	public FormComponentFetchResponse GetCustomElementComponent(FormComponentFetchRequest request) {
		return formBuilderProcessor.PerformGetCustomElementComponent(request);
	}

	@Override
	public FormComponentFetchResponse GetProgrammedElementComponent(FormComponentFetchRequest request) {
		return formBuilderProcessor.PerformGetProgrammedElementComponent(request);
	}

	@Override
	public FormComponentSaveResponse SaveQuestionnaireComponent(FormComponentSaveRequest request, 
															 	MultipartHttpServletRequest multipartRequest) {
		return formBuilderProcessor.PerformSaveQuestionnaireComponent(request, multipartRequest);
	}

	@Override
	public FormComponentSaveResponse SaveCustomElementComponent(FormComponentSaveRequest request) {
		return formBuilderProcessor.PerformSaveCustomElementComponent(request);
	}

	@Override
	public FormComponentSaveResponse SaveProgrammedElementComponent(FormComponentSaveRequest request) {
		return formBuilderProcessor.PerformSaveProgrammedElementComponent(request);
	}

	@Override
	public List<FormEvaluateValidationResponse> validateForm(FormValidationRequest formValidationRequest) {
		return formBuilderProcessor.performFormValidation(formValidationRequest);
	}

	@Override
	public List<FormEvaluateValidationResponse> validateSection(FormValidationRequest formValidationRequest) {
		return formBuilderProcessor.performSectionValidation(formValidationRequest);
	}

	@Override
	public List<FormEvaluateValidationResponse> validateComponent(FormValidationRequest formValidationRequest) {
		return formBuilderProcessor.performComponentValidation(formValidationRequest);
	}

}
