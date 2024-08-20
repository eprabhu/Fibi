package com.polus.fibicomp.globalentity.controller;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.globalentity.dto.ComplianceResponseDTO;
import com.polus.fibicomp.globalentity.dto.EntityRiskRequestDTO;
import com.polus.fibicomp.globalentity.service.ComplianceService;
import com.polus.fibicomp.globalentity.service.GlobalEntityService;

@RestController
@RequestMapping("/coi/entity/compliance")
public class ComplianceController {

	protected static Logger logger = LogManager.getLogger(ComplianceController.class.getName());

	@Autowired
	@Qualifier(value = "entityRiskService")
	private GlobalEntityService entityRiskService;

	@Autowired
	private ComplianceService complianceService;

	@PostMapping(value = "/saveRisk")
	public ResponseEntity<Map<String, Integer>> saveRisk(@RequestBody EntityRiskRequestDTO dto) {
		logger.info("Requesting for compliance/saveRisk");
		return entityRiskService.saveRisk(dto);
	}

	@PatchMapping(value = "/updateRisk")
	public ResponseEntity<String> updateRisk(@RequestBody EntityRiskRequestDTO dto) {
		logger.info("Requesting for compliance/updateRisk");
		return entityRiskService.updateRisk(dto);
	}

	@DeleteMapping(value = "/deleteRisk/{entityRiskId}")
	public ResponseEntity<String> deleteRisk(@PathVariable(value = "entityRiskId", required = true) final Integer entityRiskId) {
		logger.info("Requesting for compliance/deleteRisk");
		return entityRiskService.deleteRisk(entityRiskId);
	}

	@GetMapping(value = "/fetch/{entityId}")
	public ResponseEntity<ComplianceResponseDTO> fetchDetails(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		logger.info("Requesting for fetchDetails");
		return complianceService.fetchDetails(entityId);
	}

}
