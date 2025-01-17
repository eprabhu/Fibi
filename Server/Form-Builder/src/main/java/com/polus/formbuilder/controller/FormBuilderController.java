package com.polus.formbuilder.controller;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.appcorelib.questionnaire.dto.QuestionnaireDataBus;
import com.polus.formbuilder.customdataelement.VO.CustomDataElementVO;
import com.polus.formbuilder.customdataelement.service.CustomDataElementService;
import com.polus.formbuilder.model.ApplicableFormRequest;
import com.polus.formbuilder.model.ApplicableFormResponse;
import com.polus.formbuilder.model.BlankFormRequest;
import com.polus.formbuilder.model.BlankFormResponse;
import com.polus.formbuilder.model.FormComponentFetchRequest;
import com.polus.formbuilder.model.FormComponentFetchResponse;
import com.polus.formbuilder.model.FormComponentSaveRequest;
import com.polus.formbuilder.model.FormRequest;
import com.polus.formbuilder.model.FormResponse;
import com.polus.formbuilder.model.FormValidationRequest;
import com.polus.formbuilder.programmedelement.ProgrammedElementJSONObjectMapper;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
import com.polus.formbuilder.service.FormBuilderConstants;
import com.polus.formbuilder.service.FormBuilderServiceCoordinator;

@RestController
public class FormBuilderController {
	
	@Autowired
	FormBuilderServiceCoordinator service;
	
	@Autowired
	ProgrammedElementJSONObjectMapper PEObjectMapper;

	@Autowired
	CustomDataElementService customDataElementService;
	
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
	ResponseEntity<?> getForm(@RequestBody FormRequest request){	
		try {
			FormResponse response = service.GetForm(request);		
			return new ResponseEntity<FormResponse>(response,HttpStatus.OK);
			
		}catch(Exception e) {
			 e.printStackTrace();
			 String errorMessage = "An error occurred in /getForm request. Exception --> "+e.getMessage();
		     return new ResponseEntity<>(errorMessage, HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}
	
	@PostMapping("/getFormComponent")
	ResponseEntity<FormComponentFetchResponse> getFormComponent(@RequestBody FormComponentFetchRequest request){		
		var response = service.getFormComponent(request);		
		return new ResponseEntity<FormComponentFetchResponse>(response,HttpStatus.OK);		
	}
	
	//It was not possible to have a common save for all the component as the attachment feature
	//in questionnaire stops us to perform this, because of request structure of questionnaire with attachments.
	// so we are going with the design of having separate save for each component,
	// saveFormComponent has a dependency on implementation with the Questionnaire Engine 

    @PostMapping(value ="saveFormComponent", consumes = {MediaType.MULTIPART_FORM_DATA_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
	ResponseEntity<Object> saveFormComponent(MultipartHttpServletRequest multiRequest ){	
    	FormComponentSaveRequest request;
		try {
			request = mapFormDataToFormComponentSaveReq(multiRequest);
		} catch (IOException e) {
			return ResponseEntity
					.status(HttpStatus.BAD_REQUEST)
					.body("Invalid JSON format in /saveFormComponent request."+"\n"+e.getMessage());
		}
		try {
			var response = service.saveFormComponent(request,multiRequest);			
			return new ResponseEntity<Object>(response,HttpStatus.OK);	
		}catch(Exception e) {
			 e.printStackTrace();
			 String errorMessage = "An error occurred in /saveFormComponent save action. Exception --> "+e.getMessage();
		     return new ResponseEntity<>(errorMessage, HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}
    

    private FormComponentSaveRequest mapFormDataToFormComponentSaveReq(MultipartHttpServletRequest multiRequest) 
    throws IOException{
				
		var dto = new FormComponentSaveRequest();

        dto.setFormBuilderId(Integer.parseInt(multiRequest.getParameter("formBuilderId")));
        dto.setDocumentOwnerPersonId(multiRequest.getParameter("documentOwnerPersonId"));
        dto.setModuleItemCode(multiRequest.getParameter("moduleItemCode"));
        dto.setModuleSubItemCode(multiRequest.getParameter("moduleSubItemCode"));
        dto.setModuleItemKey(multiRequest.getParameter("moduleItemKey"));
        dto.setModuleSubItemKey(multiRequest.getParameter("moduleSubItemKey"));
        dto.setComponentId(Integer.parseInt(multiRequest.getParameter("componentId")));
        dto.setComponentType(multiRequest.getParameter("componentType"));
        dto.setComponentRefId(multiRequest.getParameter("componentRefId"));
        dto.setComponentData(multiRequest.getParameter("componentData"));
      
        ObjectMapper objectMapper = new ObjectMapper();       
	        if(dto.getComponentType().equals("QN")) {
	        	 String questionnaireJson = multiRequest.getParameter("questionnaire");
	        	 QuestionnaireDataBus questionnaire = objectMapper.readValue(questionnaireJson, QuestionnaireDataBus.class);
	        	 dto.setQuestionnaire(questionnaire);
	        	 dto.setCustomElement(null);
	        	 dto.setProgrammedElement(null);
	        } else if(FormBuilderConstants.CUSTOM_ELEMENT_COMPONENT_LIST.contains(dto.getComponentType())){
	        	String customElementJson = multiRequest.getParameter("customElement");
	        	CustomDataElementVO customElement = objectMapper.readValue(customElementJson, CustomDataElementVO.class);
	        	dto.setCustomElement(customElement);
	        	dto.setProgrammedElement(null);
	        	dto.setQuestionnaire(null);
	        }else if(dto.getComponentType().equals("PE")) {
	        	String programmedElementJson = multiRequest.getParameter("programmedElement");	        	
	        	ProgrammedElementModel programmedElement = PEObjectMapper.jsonObjectMapper(dto.getComponentData(),"SAVE_COMPONENT",programmedElementJson);
	        	dto.setProgrammedElement(programmedElement);
	        	dto.setCustomElement(null);
	        	dto.setQuestionnaire(null);
	        }
	        
	return dto;
}

    @PostMapping(value = "/validateForm", produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<Object> validateForm(@RequestBody FormValidationRequest formValidationRequest) {
		return service.validateForm(formValidationRequest);
	}

    @PostMapping(value = "/validateSection", produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<Object> validateSection(@RequestBody FormValidationRequest formValidationRequest) {
		return service.validateSection(formValidationRequest);
	}

    @PostMapping(value = "/validateComponent", produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<Object> validateComponent(@RequestBody FormValidationRequest formValidationRequest) {
		return service.validateComponent(formValidationRequest);
	}

    @PostMapping(value = "/saveFormCustomResponse", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Object> saveCustomResponse(@RequestBody CustomDataElementVO vo) {
		return new ResponseEntity<> (customDataElementService.saveCustomResponse(vo), HttpStatus.OK) ;
	}

    @PostMapping(value = "/fetchFormCustomElementById", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Object> fetchCustomElementById(@RequestBody CustomDataElementVO vo) {
		return new ResponseEntity<> (customDataElementService.fetchCustomElementById(vo), HttpStatus.OK) ;
	}

}
