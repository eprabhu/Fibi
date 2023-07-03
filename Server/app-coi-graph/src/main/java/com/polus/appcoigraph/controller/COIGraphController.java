package com.polus.appcoigraph.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Relationship;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.appcoigraph.entity.Country;
import com.polus.appcoigraph.entity.COIEntity;
import com.polus.appcoigraph.entity.GraphNode;
import com.polus.appcoigraph.model.Link;
import com.polus.appcoigraph.model.RequestDTO;
import com.polus.appcoigraph.model.ResponseDTO;
import com.polus.appcoigraph.repository.GraphRepository;
import com.polus.appcoigraph.service.COIGraphService;

import jakarta.persistence.Tuple;

@RestController
@RequestMapping("/graph")
public class COIGraphController {

	@Autowired
	private final GraphRepository graphRepository;

    public COIGraphController(GraphRepository graphRepository) {
        this.graphRepository = graphRepository;
    }
	
	@Autowired
	COIGraphService service;
	
	@GetMapping("/hello")
	public String greetings() {

		return "Hello";
	}
		
	@GetMapping("/getAllEntity")
	public List<COIEntity> getAllEntity() {
		return service.getAllEntity();
	}
	
	@GetMapping("/getAllCountry")
	public List<Country> getAllCountry() {
		return service.getAllCountry();
	}
	
    @GetMapping("/graph")
    public Map<String, Object> getGraphData() {
        Map<String, Object> graph = new HashMap<>();
        List<GraphNode> nodes = graphRepository.getAllNodes();
        List<Relationship> relationships = graphRepository.getAllRelationships();
        
        graph.put("nodes", nodes);
        graph.put("links", relationships);

        return graph;
    }
    
    @GetMapping("/custom")
    ResponseEntity<ResponseDTO> findAllCustom() { 
    	String countryCode = "USA";
    	ResponseDTO response = service.getResults();
    	return new ResponseEntity<ResponseDTO>(response, HttpStatus.OK);
    			
   }

    @GetMapping("/getLink")
    Iterable<Link> getLinks() { 
    	String countryCode = "AFG";
    	Iterable<Link> data = service.getLinks(countryCode);
    	String hello = "hrllo";
    	return data;
    	}

    @GetMapping("/import")
    String importFromRDBMS() { 
    		service.importDataFromRDBMS();
    	  	return "Success";
    	} 
    
    @GetMapping("/test")
    String test() { 
    		service.executeCypherQuery();
    	  	return "Success";
    	} 
    
    @PostMapping("/entitygraph")
    ResponseEntity<ResponseDTO> entityGraph(@RequestBody RequestDTO request) { 
	 ResponseDTO response = service.entityGraph(request);
	 return new ResponseEntity<ResponseDTO>(response, HttpStatus.OK);
  }   
    
    
}

