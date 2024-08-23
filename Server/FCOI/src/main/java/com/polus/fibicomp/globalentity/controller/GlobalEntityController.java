package com.polus.fibicomp.globalentity.controller;

import java.util.List;
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

import com.polus.core.pojo.Currency;
import com.polus.fibicomp.globalentity.dto.AddressDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityResponseDTO;
import com.polus.fibicomp.globalentity.dto.EntityRiskRequestDTO;
import com.polus.fibicomp.globalentity.dto.ExternalReferenceRequestDTO;
import com.polus.fibicomp.globalentity.dto.ForeignNameRequestDTO;
import com.polus.fibicomp.globalentity.dto.ForeignNameResponseDTO;
import com.polus.fibicomp.globalentity.dto.IndustryDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.OtherDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.PriorNameRequestDTO;
import com.polus.fibicomp.globalentity.dto.PriorNameResponseDTO;
import com.polus.fibicomp.globalentity.dto.RegistrationDetailsRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntityIndustryClassification;
import com.polus.fibicomp.globalentity.pojo.EntityRiskType;
import com.polus.fibicomp.globalentity.pojo.IndustryCategoryCode;
import com.polus.fibicomp.globalentity.service.GlobalEntityService;

@RestController
@RequestMapping("/coi/entity")
public class GlobalEntityController {

	protected static Logger logger = LogManager.getLogger(GlobalEntityController.class.getName());

	@Autowired
	@Qualifier(value = "globalEntityService")
	private GlobalEntityService globalEntityService;

	@Autowired
	@Qualifier(value = "entityDetailsService")
	private GlobalEntityService entityDetailsService;

	@Autowired
	@Qualifier(value = "companyDetailsService")
	private GlobalEntityService companyDetailsService;

	@Autowired
	@Qualifier(value = "entityRiskService")
	private GlobalEntityService entityRiskService;

	@Autowired
	@Qualifier(value = "entityExternalReferenceService")
	private GlobalEntityService entityExternalReferenceService;

	@PostMapping(value = "/create")
	public ResponseEntity<Map<String, Integer>> createEntity(@RequestBody EntityRequestDTO dto) {
		logger.info("Requesting for createEntity");
		return entityDetailsService.createEntity(dto);
	}

	@PatchMapping(value = "/update")
	public ResponseEntity<String> updateEntityDetails(@RequestBody EntityRequestDTO dto) {
		logger.info("Requesting for updateEntityDetails");
		return entityDetailsService.updateEntityDetails(dto);
	}

	@GetMapping(value = "/fetch/{entityId}")
	public ResponseEntity<EntityResponseDTO> fetchEntityDetails(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		logger.info("Requesting for fetchEntityDetails");
		return entityDetailsService.fetchEntityDetails(entityId);
	}

	@GetMapping(value = "/fetchIndustryCategoryCode/{industryCategroyTypeCode}")
	public ResponseEntity<List<IndustryCategoryCode>> fetchIndustryCategoryCode(@PathVariable(value = "industryCategroyTypeCode", required = true) final String industryCategroyTypeCode) {
		logger.info("Requesting for fetchIndustryCategoryCode");
		return companyDetailsService.fetchIndustryCategoryCode(industryCategroyTypeCode);
	}
	
	@PostMapping(value = "/saveIndustryDetails")
	public ResponseEntity<List<EntityIndustryClassification>> saveIndustryDetails(@RequestBody IndustryDetailsRequestDTO dto) {
		logger.info("Requesting for saveIndustryDetails");
		companyDetailsService.saveIndustryDetails(dto);
		return companyDetailsService.fetchIndustryDetails(dto.getEntityId());
	}

	@PatchMapping(value = "/updateIndustryDetails")
	public ResponseEntity<String> updateIndustryDetails(@RequestBody IndustryDetailsRequestDTO dto) {
		logger.info("Requesting for updateIndustryDetails");
		return companyDetailsService.updateIndustryDetails(dto);
	}

	@DeleteMapping(value = "/deleteIndustryDetails/{entityIndustryClassId}")
	public ResponseEntity<String> deleteIndustryDetails(@PathVariable(value = "entityIndustryClassId", required = true) final Integer entityIndustryClassId) {
		logger.info("Requesting for deleteIndustryDetails");
		return companyDetailsService.deleteIndustryDetails(entityIndustryClassId);
	}

	@PostMapping(value = "/saveRegistrationDetails")
	public ResponseEntity<Map<String, Integer>> saveRegistrationDetails(@RequestBody RegistrationDetailsRequestDTO dto) {
		logger.info("Requesting for saveRegistrationDetails");
		return companyDetailsService.saveRegistrationDetails(dto);
	}

	@PatchMapping(value = "/updateRegistrationDetails")
	public ResponseEntity<String> updateRegistrationDetails(@RequestBody RegistrationDetailsRequestDTO dto) {
		logger.info("Requesting for updateRegistrationDetails");
		return companyDetailsService.updateRegistrationDetails(dto);
	}

	@DeleteMapping(value = "/deleteRegistrationDetails/{entityRegistrationId}")
	public ResponseEntity<String> deleteRegistrationDetails(@PathVariable(value = "entityRegistrationId", required = true) final Integer entityRegistrationId) {
		logger.info("Requesting for deleteRegistrationDetails");
		return companyDetailsService.deleteRegistrationDetails(entityRegistrationId);
	}

	@PostMapping(value = "/saveAdditionalAddresses")
	public ResponseEntity<Map<String, Integer>> saveAdditionalAddresses(@RequestBody AddressDetailsRequestDTO dto) {
		logger.info("Requesting for saveAdditionalAddresses");
		return companyDetailsService.saveAdditionalAddresses(dto);
	}

	@PatchMapping(value = "/updateAdditionalAddresses")
	public ResponseEntity<String> updateAdditionalAddresses(@RequestBody AddressDetailsRequestDTO dto) {
		logger.info("Requesting for updateAdditionalAddresses");
		return companyDetailsService.updateAdditionalAddresses(dto);
	}

	@DeleteMapping(value = "/deleteAdditionalAddress/{entityMailingAddressId}")
	public ResponseEntity<String> deleteAdditionalAddress(@PathVariable(value = "entityMailingAddressId", required = true) final Integer entityMailingAddressId) {
		logger.info("Requesting for deleteAdditionalAddress");
		return companyDetailsService.deleteAdditionalAddress(entityMailingAddressId);
	}

	@PatchMapping(value = "/updateOtherDetails")
	public ResponseEntity<String> updateOtherDetails(@RequestBody OtherDetailsRequestDTO dto) {
		logger.info("Requesting for updateOtherDetails");
		return companyDetailsService.updateOtherDetails(dto);
	}

	@PostMapping(value = "/saveRisk")
	public ResponseEntity<Map<String, Integer>> saveRisk(@RequestBody EntityRiskRequestDTO dto) {
		logger.info("Requesting for saveRisk");
		return entityRiskService.saveRisk(dto);
	}

	@PatchMapping(value = "/updateRisk")
	public ResponseEntity<String> updateRisk(@RequestBody EntityRiskRequestDTO dto) {
		logger.info("Requesting for updateRisk");
		return entityRiskService.updateRisk(dto);
	}

	@DeleteMapping(value = "/deleteRisk/{entityRiskId}")
	public ResponseEntity<String> deleteRisk(@PathVariable(value = "entityRiskId", required = true) final Integer entityRiskId) {
		logger.info("Requesting for deleteRisk");
		return entityRiskService.deleteRisk(entityRiskId);
	}

	@PostMapping(value = "/saveExternalReference")
	public ResponseEntity<Map<String, Integer>> saveExternalReference(@RequestBody ExternalReferenceRequestDTO dto) {
		logger.info("Requesting for saveExternalReference");
		return entityExternalReferenceService.saveExternalReference(dto);
	}

	@PatchMapping(value = "/updateExternalReference")
	public ResponseEntity<String> updateExternalReference(@RequestBody ExternalReferenceRequestDTO dto) {
		logger.info("Requesting for updateExternalReference");
		return entityExternalReferenceService.updateExternalReference(dto);
	}

	@DeleteMapping(value = "/deleteExternalReference/{entityExternalMappingId}")
	public ResponseEntity<String> deleteExternalReference(@PathVariable(value = "entityExternalMappingId", required = true) final Integer entityExternalMappingId) {
		logger.info("Requesting for deleteExternalReference");
		return entityExternalReferenceService.deleteExternalReference(entityExternalMappingId);
	}

	@PostMapping(value = "/dunsNumberExists")
	public ResponseEntity<Boolean> dunsNumberExists(@RequestBody EntityRequestDTO dto) {
		logger.info("Requesting for dunsNumberExists");
		return globalEntityService.isDunsNumberExists(dto.getDunsNumber());
	}

	@PostMapping(value = "/ueiNumberExists")
	public ResponseEntity<Boolean> ueiNumberExists(@RequestBody EntityRequestDTO dto) {
		logger.info("Requesting for ueiNumberExists");
		return globalEntityService.isUeiNumberExists(dto.getUeiNumber());
	}

	@PostMapping(value = "/cageNumberExists")
	public ResponseEntity<Boolean> cageNumberExists(@RequestBody EntityRequestDTO dto) {
		logger.info("Requesting for cageNumberExists");
		return globalEntityService.isCageNumberExists(dto.getCageNumber());
	}

	@GetMapping(value = "/fetchCurrencyDetails")
	public ResponseEntity<List<Currency>> fetchCurrencyDetails() {
		logger.info("Requesting for fetchCurrencyDetails");
		return globalEntityService.fetchCurrencyDetails();
	}

	@PostMapping(value = "/addPriorName")
	public ResponseEntity<Map<String, Integer>> addPriorName(@RequestBody PriorNameRequestDTO dto) {
		logger.info("Requesting for addPriorName");
		return companyDetailsService.addPriorName(dto);
	}

	@GetMapping(value = "/fetchPriorNames/{entityId}")
	public List<PriorNameResponseDTO> fetchPriorNames(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		logger.info("Requesting for fetchPriorNames");
		return companyDetailsService.fetchPriorNames(entityId);
	}

	@DeleteMapping(value = "/deletePriorName/{id}")
	public ResponseEntity<String> deletePriorName(@PathVariable(value = "id", required = true) final Integer id) {
		logger.info("Requesting for deletePriorName");
		return companyDetailsService.deletePriorName(id);
	}

	@PostMapping(value = "/addForeignName")
	public ResponseEntity<Map<String, Integer>> addForeignName(@RequestBody ForeignNameRequestDTO dto) {
		logger.info("Requesting for addForeignName");
		return companyDetailsService.addForeignName(dto);
	}

	@GetMapping(value = "/fetchForeignNames/{entityId}")
	public List<ForeignNameResponseDTO> fetchForeignNames(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		logger.info("Requesting for fetchForeignNames");
		return companyDetailsService.fetchForeignNames(entityId);
	}

	@DeleteMapping(value = "/deleteForeignName/{id}")
	public ResponseEntity<String> deleteForeignName(@PathVariable(value = "id", required = true) final Integer id) {
		logger.info("Requesting for deleteForeignName");
		return companyDetailsService.deleteForeignName(id);
	}

	@GetMapping(value = "/fetchRiskTypes")
	public ResponseEntity<List<EntityRiskType>> fetchRiskTypes() {
		logger.info("Requesting for fetchRiskTypes");
		return entityRiskService.fetchRiskTypes();
	}

}
