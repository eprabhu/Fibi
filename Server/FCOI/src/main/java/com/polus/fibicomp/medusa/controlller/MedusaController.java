package com.polus.fibicomp.medusa.controlller;

import java.util.ArrayList;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.polus.fibicomp.claims.vo.ClaimsVO;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.PostMapping;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.medusa.dto.Medusa;
import com.polus.fibicomp.medusa.dto.MedusaDTO;
import com.polus.fibicomp.medusa.service.MedusaService;

@RestController
public class MedusaController {

	protected static Logger logger = LogManager.getLogger(MedusaController.class.getName());

	@Autowired
	@Qualifier(value = "medusaService")
	MedusaService medusaService;

	@RequestMapping(value = "/getMedusa", method = RequestMethod.POST)
	public ResponseEntity<String> getMedusa(HttpServletRequest request, HttpServletResponse response, @RequestBody Medusa medusa) throws Exception {
		logger.info("Requesting for getMedusa");
		ObjectMapper mapper = new ObjectMapper();
		HttpStatus status = HttpStatus.OK;
		Integer moduleCode = medusa.getModuleCode();
		String projectId = medusa.getProjectId();
		MedusaDTO medusaDto = medusaService.getMedusa(moduleCode, projectId);
		ArrayList<MedusaDTO> medusaDTOs = new ArrayList<>();
		medusaDTOs.add(medusaDto);
		return new ResponseEntity<String>(mapper.writeValueAsString(medusaDTOs), status);
	}

	@RequestMapping(value = "/getMedusaMoreDetail", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getMedusaMoreDetail(@RequestBody MedusaDTO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getMedusaMoreDetail");
		return medusaService.getMedusaMoreDetail(vo);
	}
	@PostMapping(value = "/getServiceRequestDetailsForMedusaByModule")
	public String getModuleServiceRequestDetail(@RequestBody Medusa medusa){
		logger.info("Requesting for getModuleServiceRequestDetail");
		return medusaService.getModuleServiceRequestDetail(medusa);

	}
}
