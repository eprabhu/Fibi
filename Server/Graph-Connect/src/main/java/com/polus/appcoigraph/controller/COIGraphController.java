package com.polus.appcoigraph.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.appcoigraph.model.RequestDTO;
import com.polus.appcoigraph.model.ResponseDTO;
import com.polus.appcoigraph.service.COIGraphService;

@RestController
@RequestMapping("/coi")
public class COIGraphController {
	
	@Autowired
	COIGraphService service;
	
	@GetMapping("/ping")
	public String greetings() {
		return "Hello from Graph Connect";
	}			

    @GetMapping("/import")
    String importFromRDBMS() { 
    		service.importDataFromRDBMS();
    	  	return "Success";
    }    
    
    @PostMapping("/entitygraph")
    ResponseEntity<ResponseDTO> entityGraph(@RequestBody RequestDTO request) { 
	 ResponseDTO response = service.entityGraph(request);
	 return new ResponseEntity<ResponseDTO>(response, HttpStatus.OK);
  }   
    
    
}

