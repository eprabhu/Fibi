package com.polus.formbuilder.formconfig.v1.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.formbuilder.formconfig.v1.model.FormDataResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderCreateModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderUpdateModel;
import com.polus.formbuilder.formconfig.v1.model.FormBasicCommonModel;
import com.polus.formbuilder.formconfig.v1.model.FormDashboardResponseModel;
import com.polus.formbuilder.formconfig.v1.service.FormBuilderConfigurationService;

@RestController
@RequestMapping("/config/v1/")
public class FormBuilderConfigurationController {

	@Autowired
	FormBuilderConfigurationService service;
	
 	@GetMapping("/formlist")
    public ResponseEntity<?> getFormList() {        
 		List<FormDashboardResponseModel> response = service.fetchFormList();
        return new ResponseEntity<List<FormDashboardResponseModel>>(response,HttpStatus.OK);
    }
 	 	
 	@GetMapping("/form/{formBuilderId}")
    public ResponseEntity<?> getFormById(@PathVariable("formBuilderId") Integer formBuilderId) {
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
        return new ResponseEntity<String>(result,HttpStatus.OK);
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
        return new ResponseEntity<String>(response,HttpStatus.OK);
    }
}
