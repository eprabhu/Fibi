package com.polus.fibicomp.mapmaintenance.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.mapmaintenance.dto.MapMaintenanceDataBus;
import com.polus.fibicomp.mapmaintenance.service.MapMaintenanceService;

@Controller
public class MapMaintenanceController {

	@Autowired
	MapMaintenanceService mapMaintenanceService;
	
	MapMaintenanceDataBus mapMaintenanceDataBus = new MapMaintenanceDataBus();

	@RequestMapping(value = "/getMapList", method = RequestMethod.GET)
	public ResponseEntity<String> getMapList(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		mapMaintenanceDataBus = mapMaintenanceService.getMapList(mapMaintenanceDataBus);
		String responseData = mapper.writeValueAsString(mapMaintenanceDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getRoleDescription", method = RequestMethod.GET)
	public ResponseEntity<String> getMapDetails(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		mapMaintenanceDataBus = mapMaintenanceService.getRoleDescription(mapMaintenanceDataBus);
		String responseData = mapper.writeValueAsString(mapMaintenanceDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getUnitLists", method = RequestMethod.GET)
	public ResponseEntity<String> getUnitList(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		mapMaintenanceDataBus = mapMaintenanceService.getUnitList(mapMaintenanceDataBus);
		String responseData = mapper.writeValueAsString(mapMaintenanceDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getMapDetailsById", method = RequestMethod.POST)
	public ResponseEntity<String> viewMapDetails(@RequestBody MapMaintenanceDataBus mapMaintenanceDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		System.out.println();
		ObjectMapper mapper = new ObjectMapper();
		try {
			mapMaintenanceDataBus = mapMaintenanceService.getMapDetailsId(mapMaintenanceDataBus);
		} catch (Exception e) {
			e.printStackTrace();
		}
		String responseData = mapper.writeValueAsString(mapMaintenanceDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/deleteMap", method = RequestMethod.POST)
	public ResponseEntity<String> inactivateMap(@RequestBody MapMaintenanceDataBus mapMaintenanceDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;		
		ObjectMapper mapper = new ObjectMapper();
		mapMaintenanceDataBus = mapMaintenanceService.deleteMap(mapMaintenanceDataBus);
		String responseData = mapper.writeValueAsString(mapMaintenanceDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/insertMap", method = RequestMethod.POST)
	public ResponseEntity<String> insertMap(@RequestBody MapMaintenanceDataBus mapMaintenanceDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		mapMaintenanceDataBus = mapMaintenanceService.insertMap(mapMaintenanceDataBus);
		String responseData = mapper.writeValueAsString(mapMaintenanceDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/updateMap", method = RequestMethod.POST)
	public ResponseEntity<String> updateMap(@RequestBody MapMaintenanceDataBus mapMaintenanceDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		mapMaintenanceDataBus = mapMaintenanceService.updateMap(mapMaintenanceDataBus);
		String responseData = mapper.writeValueAsString(mapMaintenanceDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getRoleDescriptionByModuleCode", method = RequestMethod.POST)
	public ResponseEntity<String> getRoleDescriptionByModuleCode(@RequestBody MapMaintenanceDataBus mapMaintenanceDataBus, HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		mapMaintenanceDataBus = mapMaintenanceService.getRoleDescriptionByModuleCode(mapMaintenanceDataBus);
		String responseData = mapper.writeValueAsString(mapMaintenanceDataBus);
		return new ResponseEntity<String>(responseData, status);
	}
}
