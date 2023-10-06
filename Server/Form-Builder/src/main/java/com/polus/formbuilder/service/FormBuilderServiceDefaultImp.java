package com.polus.formbuilder.service;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.polus.formbuilder.model.BlankFormRequest;
import com.polus.formbuilder.model.BlankFormResponse;
import com.polus.formbuilder.model.FormComponentFetchRequest;
import com.polus.formbuilder.model.FormComponentFetchResponse;
import com.polus.formbuilder.model.FormComponentSaveRequest;
import com.polus.formbuilder.model.FormComponentSaveResponse;
import com.polus.formbuilder.model.FormRequest;
import com.polus.formbuilder.model.FormResponse;

@Service
public class FormBuilderServiceDefaultImp implements FormBuilderService {

    private final FormBuilderServiceProcessor formBuilderProcessor;

    public FormBuilderServiceDefaultImp(FormBuilderServiceProcessor formBuilderProcessor) {
        this.formBuilderProcessor = formBuilderProcessor;
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


}
