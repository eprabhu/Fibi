package com.polus.formbuilder.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
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
import com.polus.formbuilder.service.FormBuilderServiceCoordinator;

@RestController
@RequestMapping("/")
public class FormBuilderController {

	@Autowired
	FormBuilderServiceCoordinator service;
	
	@GetMapping("/ping")
	String greetings() {
		return "Hello from Form Builder App. "+Runtime.getRuntime().availableProcessors();
	}	
	
	@PostMapping("/getApplicableForms")
	ResponseEntity<ApplicableFormResponse> getApplicableForms(@RequestBody ApplicableFormRequest request){		
		var response = service.getApplicableForms(request);		
		return new ResponseEntity<ApplicableFormResponse>(response,HttpStatus.OK);		
	}
	
	@PostMapping("/getBlankForm")
	ResponseEntity<BlankFormResponse> getBlankForm(@RequestBody BlankFormRequest request){		
		var response = service.GetBankForm(request);		
		return new ResponseEntity<BlankFormResponse>(response,HttpStatus.OK);		
	}
	
//	@PostMapping("/getBlankFormByFormId")
//	ResponseEntity<BlankFormResponse> getBlankFormByFormId(@RequestBody Integer formId){		
//		//var response = service.getBlankFormByFormId(formId);		
//		return new ResponseEntity<BlankFormResponse>(new BlankFormResponse(),HttpStatus.OK);		
//	}
	
	@PostMapping("/getForm")
	ResponseEntity<FormResponse> getForm(@RequestBody FormRequest request){		
		FormResponse response = service.GetForm(request);		
		return new ResponseEntity<FormResponse>(response,HttpStatus.OK);		
	}
	
	@PostMapping("/getFormComponent")
	ResponseEntity<FormComponentFetchResponse> getFormComponent(@RequestBody FormComponentFetchRequest request){		
		var response = service.getFormComponent(request);		
		return new ResponseEntity<FormComponentFetchResponse>(response,HttpStatus.OK);		
	}
	
	//It was not possible to have a common save for all the component as the attachment feature
	//in questionnaire stops us to perform this, because of request structure of questionnaire with attachments.
	// so we are going with the design of having separate save for each component,

//	@PostMapping("/saveFormComponent")
//	ResponseEntity<List<FormComponentSaveResponse>> saveFormComponent(@RequestBody FormComponentSaveRequest request){		
//		var response = service.saveFormComponent(request);		
//		return new ResponseEntity<List<FormComponentSaveResponse>>(response,HttpStatus.OK);		
//	}
	
	@PostMapping("/saveFormComponent")
	ResponseEntity<FormComponentSaveResponse> 
								saveFormComponent(@RequestBody FormComponentSaveRequest request,
												  MultipartHttpServletRequest multipartRequest
												  ){		
		var response = service.saveFormComponent(request,multipartRequest);	
		return new ResponseEntity<FormComponentSaveResponse>(response,HttpStatus.OK);		
	}
	
//	@PostMapping("/saveQuestionnaireComponent")
//	ResponseEntity<FormComponentSaveResponse> saveQuestionnaireComponent(@RequestBody FormComponentSaveRequest request){		
//		var response = service.saveQuestionnaireComponent(request);		
//		return new ResponseEntity<FormComponentSaveResponse>(response,HttpStatus.OK);		
//	}
//	
//	@PostMapping("/saveCustomElementComponent")
//	ResponseEntity<FormComponentSaveResponse> saveCustomElementComponent(@RequestBody FormComponentSaveRequest request){		
//		var response = service.saveCustomElementComponent(request);		
//		return new ResponseEntity<FormComponentSaveResponse>(response,HttpStatus.OK);	
//	}
//	
//	@PostMapping("/saveProgrammedElementComponent")
//	ResponseEntity<FormComponentSaveResponse> saveProgrammedElementComponent(@RequestBody FormComponentSaveRequest request){		
//		var response = service.saveProgrammedElementComponent(request);		
//		return new ResponseEntity<FormComponentSaveResponse>(response,HttpStatus.OK);	
//	}
//	
//	@PostMapping(value = "/saveQuestionnaire")
//	public ResponseEntity<String> saveQuestionnaire(MultipartHttpServletRequest request, HttpServletResponse reponse) {
//		ObjectMapper mapper = new ObjectMapper();
//		QuestionnaireDataBus questionnaireDataBus = null;
//		String formDataJson = request.getParameter("formDataJson");
//		try {
//		questionnaireDataBus = mapper.readValue(formDataJson, QuestionnaireDataBus.class);
//		} catch (Exception e) {
//			
//		}
//		//return questionnaireService.saveQuestionnaireAnswers(questionnaireDataBus, request);
//		return null;
//	}
	
	
}
