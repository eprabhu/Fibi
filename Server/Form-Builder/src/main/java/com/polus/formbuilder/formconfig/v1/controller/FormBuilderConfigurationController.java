package com.polus.formbuilder.formconfig.v1.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.appcorelib.customdataelement.service.CustomDataElementService;
import com.polus.appcorelib.customdataelement.vo.CustomDataElementVO;
import com.polus.appcorelib.questionnaire.dto.QuestionnaireDataBus;
import com.polus.appcorelib.questionnaire.service.QuestionnaireService;
import com.polus.formbuilder.entity.FormBuilderProgElementEntity;
import com.polus.formbuilder.formconfig.v1.model.DeleteResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormBasicCommonModel;
import com.polus.formbuilder.formconfig.v1.model.FormComponentRequestModel;
import com.polus.formbuilder.formconfig.v1.model.FormDashboardResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormDataResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderCreateModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderUpdateModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionComponentModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionRequestModel;
import com.polus.formbuilder.formconfig.v1.model.FormUsageModel;
import com.polus.formbuilder.formconfig.v1.model.FormUsageRequestModel;
import com.polus.formbuilder.formconfig.v1.model.OrderUpdateResponseModel;
import com.polus.formbuilder.formconfig.v1.service.FormBuilderConfigurationService;
import com.polus.formbuilder.programmedelement.ProgrammedElementService;

@RestController
@RequestMapping("/config/v1/")
public class FormBuilderConfigurationController {

	@Autowired
	FormBuilderConfigurationService service;

	@Autowired
	private QuestionnaireService questionnaireService;

	@Autowired
	private ProgrammedElementService programmedElementService;

	@Autowired
	private CustomDataElementService customDataElementService;

 	@GetMapping("/formlist")
    public ResponseEntity<?> getFormList() {        
 		List<FormDashboardResponseModel> response = service.fetchFormList();
        return new ResponseEntity<List<FormDashboardResponseModel>>(response,HttpStatus.OK);
    }
 	 	
 	@GetMapping("/form/{formBuilderId}")
    public ResponseEntity<?> fetchFormById(@PathVariable("formBuilderId") Integer formBuilderId) {
        FormDataResponseModel response = service.fetchFormById(formBuilderId);
        return new ResponseEntity<FormDataResponseModel>(response,HttpStatus.OK);
    }
 	
 	@PostMapping("/copyform")
    public ResponseEntity<?> copyForm(@RequestBody FormBasicCommonModel request) {
        FormDataResponseModel response = service.copyForm(request.formBuilderId());
        
        return new ResponseEntity<FormDataResponseModel>(response,HttpStatus.OK);
    }
 	
 	@DeleteMapping("/deleteform")
    public ResponseEntity<?> deleteForm(@RequestBody FormBasicCommonModel request) {
        String result = service.deleteForm(request.formBuilderId());
        return new ResponseEntity<DeleteResponseModel>(new DeleteResponseModel(result),HttpStatus.OK);
    }
 
 	
 	
 //FORM HEADER ---------------------------------------
 	
 	@PostMapping("/formheader")
    public ResponseEntity<?> createFormHeader(@RequestBody FormHeaderCreateModel request) {
 		FormHeaderModel response = service.createHeader(request);        
        return new ResponseEntity<FormHeaderModel>(response,HttpStatus.OK);
    }
 	
 	@PutMapping("/formheader")
    public ResponseEntity<?> updateFormHeader(@RequestBody FormHeaderUpdateModel request) {
 		FormHeaderModel response = service.updateHeader(request);        
        return new ResponseEntity<FormHeaderModel>(response,HttpStatus.OK);
    }
 	
 	@DeleteMapping("/formheader")
    public ResponseEntity<?> deleteFormHeader(@RequestBody FormBasicCommonModel request) {
 		String response = service.deleteHeader(request.formBuilderId());        
        return new ResponseEntity<DeleteResponseModel>(new DeleteResponseModel(response),HttpStatus.OK);
    }
 	
 	@GetMapping("/formheader/{formBuilderId}")
    public ResponseEntity<?> fetchFormHeader(@PathVariable("formBuilderId") Integer formBuilderId) {
 		FormHeaderModel response = service.fetchFormHeader(formBuilderId);
        return new ResponseEntity<FormHeaderModel>(response,HttpStatus.OK);
    }
 	
 
 	
 	
 
 //FORM USAGE ---------------------------------------
 	
 	@PostMapping("/formusage")
    public ResponseEntity<?> createFormUsage(@RequestBody FormUsageRequestModel request) {
 		FormUsageModel response = service.createFormUsage(request);        
        return new ResponseEntity<FormUsageModel>(response,HttpStatus.OK);
    }
 	
 	@PutMapping("/formusage")
    public ResponseEntity<?> updateFormUsage(@RequestBody FormUsageRequestModel request) {
 		FormUsageModel response = service.updateFormUsage(request);        
        return new ResponseEntity<FormUsageModel>(response,HttpStatus.OK);
    }
 	
 	@DeleteMapping("/formusage")
    public ResponseEntity<?> deleteFormUsage(@RequestBody FormUsageRequestModel request) {
 		String response = service.deleteFormUsage(request.getFormUsageId());        
        return new ResponseEntity<DeleteResponseModel>(new DeleteResponseModel(response),HttpStatus.OK);
    }
 	
 	@GetMapping("/formusage/{formBuilderId}")
    public ResponseEntity<?> fetchFormUsage(@PathVariable("formBuilderId") Integer formBuilderId) {
 		List<FormUsageModel> response = service.fetchFormUsage(formBuilderId);
        return new ResponseEntity<List<FormUsageModel>>(response,HttpStatus.OK);
    }
 		
 	@PatchMapping("/formusage/order")
    public ResponseEntity<?> updateUsageOrder(@RequestBody List<FormUsageRequestModel> request) {
 		String response = service.updateUsageOrder(request);        
        return new ResponseEntity<OrderUpdateResponseModel>(new OrderUpdateResponseModel(response),HttpStatus.OK);
    }
 
 	
 	
 //FORM SECTION ---------------------------------------
 	
 	@GetMapping("/allformsections/{formBuilderId}")
    public ResponseEntity<?> fetchAllFormSection(@PathVariable("formBuilderId") Integer formBuilderId) {
 		List<FormSectionModel> response = service.fetchAllFormSection(formBuilderId);
        return new ResponseEntity<List<FormSectionModel>>(response,HttpStatus.OK);
    }
 	
 	@PostMapping("/formsection")
    public ResponseEntity<?> createFormSection(@RequestBody FormSectionRequestModel request) {
 		FormSectionModel response = service.createFormSection(request);        
        return new ResponseEntity<FormSectionModel>(response,HttpStatus.OK);
    }
 	
 	@PutMapping("/formsection")
    public ResponseEntity<?> updateFormSection(@RequestBody FormSectionRequestModel request) {
 		FormSectionModel response = service.updateFormSection(request);        
        return new ResponseEntity<FormSectionModel>(response,HttpStatus.OK);
    }
 	
 	@DeleteMapping("/formsection/{formBuilderSectId}")
    public ResponseEntity<?> deleteFormSection(@PathVariable("formBuilderSectId") Integer formBuilderSectId) {
 		String response = service.deleteFormSection(formBuilderSectId);        
        return new ResponseEntity<DeleteResponseModel>(new DeleteResponseModel(response),HttpStatus.OK);
    }
 	
 	@GetMapping("/formsection/{formBuilderSectionId}")
    public ResponseEntity<?> fetchFormSection(@PathVariable("formBuilderSectionId") Integer formBuilderSectionId) {
 		FormSectionModel response = service.fetchFormSection(formBuilderSectionId);
        return new ResponseEntity<FormSectionModel>(response,HttpStatus.OK);
    }
 		
 	@PatchMapping("/formsection/order")
    public ResponseEntity<?> updateSectionOrder(@RequestBody List<FormSectionRequestModel> request) {
 		String response = service.updateSectionOrder(request);        
        return new ResponseEntity<OrderUpdateResponseModel>(new OrderUpdateResponseModel(response),HttpStatus.OK);
    }
 
  	
	
 //FORM COMPONENT ---------------------------------------
 	
 	@GetMapping("/allsectioncomponents/{formBuilderSectionId}")
    public ResponseEntity<?> fetchAllFormComponent(@PathVariable("formBuilderSectionId") Integer formBuilderSectionId) {
 		List<FormSectionComponentModel> response = service.fetchAllFormComponent(formBuilderSectionId);
        return new ResponseEntity<List<FormSectionComponentModel>>(response,HttpStatus.OK);
    }
 	
 	@PostMapping("/sectioncomponent")
    public ResponseEntity<?> createFormComponent(@RequestBody FormComponentRequestModel request) {
 		FormSectionComponentModel response = service.createFormComponent(request);        
        return new ResponseEntity<FormSectionComponentModel>(response,HttpStatus.OK);
    }
 	
 	@PutMapping("/sectioncomponent")
    public ResponseEntity<?> updateFormComponent(@RequestBody FormComponentRequestModel request) {
 		FormSectionComponentModel response = service.updateFormComponent(request);        
        return new ResponseEntity<FormSectionComponentModel>(response,HttpStatus.OK);
    }
 	
 	@DeleteMapping("/sectioncomponent/{formBuilderSectCompId}")
    public ResponseEntity<?> deleteFormComponent(@PathVariable("formBuilderSectCompId") Integer formBuilderSectCompId) {
 		String response = service.deleteFormComponent(formBuilderSectCompId);        
        return new ResponseEntity<DeleteResponseModel>(new DeleteResponseModel(response),HttpStatus.OK);
    }
 	
 	@GetMapping("/sectioncomponent/{formBuilderSectCompId}")
    public ResponseEntity<?> fetchFormComponent(@PathVariable("formBuilderSectCompId") Integer formBuilderSectCompId) {
 		FormSectionComponentModel response = service.fetchFormComponent(formBuilderSectCompId);
        return new ResponseEntity<FormSectionComponentModel>(response,HttpStatus.OK);
    }
 		
 	@PatchMapping("/sectioncomponent/order")
    public ResponseEntity<?> updateComponentOrder(@RequestBody List<FormComponentRequestModel> request) {
 		String response = service.updateComponentOrder(request);        
        return new ResponseEntity<OrderUpdateResponseModel>(new OrderUpdateResponseModel(response),HttpStatus.OK);
    }

 // QUESTIONNAIRE CONFIG ---------------------------------------
 	@GetMapping("/questionnaireList")
 	public ResponseEntity<?> fetchquestionnaireList() {
 		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
 		QuestionnaireDataBus response = questionnaireService.showAllQuestionnaire(questionnaireDataBus);
//  			response.getQuestionnaireList().removeIf(questionnaire -> "N".equals(questionnaire.get("IS_FINAL")));
 		return new ResponseEntity<>(response, HttpStatus.OK);
 	}

 	// PE CONFIG ---------------------------------------
 	@GetMapping("/programmedElementList")
 	public ResponseEntity<?> fetchprogrammedElementListList() {
 		List<FormBuilderProgElementEntity> response = programmedElementService.getAllProgrammedElement();
 		return new ResponseEntity<>(response, HttpStatus.OK);
 	}

 	// CE CONFIG ---------------------------------------
 	@GetMapping("/customElementList")
 	public ResponseEntity<?> fetchCustomElementList() {
 		CustomDataElementVO vo = new CustomDataElementVO();
 		String response = customDataElementService.fetchAllCustomElement(vo);
 		return new ResponseEntity<>(response, HttpStatus.OK);
 	}

}
