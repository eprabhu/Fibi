package com.polus.fibicomp.globalentity.controller;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
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
import com.polus.fibicomp.coi.dto.EntityActionLogDto;
import com.polus.fibicomp.globalentity.dto.ActionLogRequestDTO;
import com.polus.fibicomp.globalentity.dto.AddressDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityResponseDTO;
import com.polus.fibicomp.globalentity.dto.EntityRiskActionLogResponseDTO;
import com.polus.fibicomp.globalentity.dto.EntityRiskRequestDTO;
import com.polus.fibicomp.globalentity.dto.ExternalReferenceRequestDTO;
import com.polus.fibicomp.globalentity.dto.ForeignNameRequestDTO;
import com.polus.fibicomp.globalentity.dto.ForeignNameResponseDTO;
import com.polus.fibicomp.globalentity.dto.IndustryDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.MarkDuplicateRequestDTO;
import com.polus.fibicomp.globalentity.dto.OtherDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.PriorNameRequestDTO;
import com.polus.fibicomp.globalentity.dto.PriorNameResponseDTO;
import com.polus.fibicomp.globalentity.dto.RegistrationDetailsRequestDTO;
import com.polus.fibicomp.globalentity.dto.ResponseMessageDTO;
import com.polus.fibicomp.globalentity.dto.ValidateDuplicateRequestDTO;
import com.polus.fibicomp.globalentity.dto.validateDuplicateResponseDTO;
import com.polus.fibicomp.globalentity.pojo.EntityIndustryClassification;
import com.polus.fibicomp.globalentity.pojo.EntityRiskLevel;
import com.polus.fibicomp.globalentity.pojo.EntityRiskType;
import com.polus.fibicomp.globalentity.pojo.IndustryCategoryCode;
import com.polus.fibicomp.globalentity.service.GlobalEntityService;

import lombok.extern.slf4j.Slf4j;

@RestController
@RequestMapping("/coi/entity")
@Slf4j
public class GlobalEntityController {

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
		log.info("Requesting for createEntity");
		return entityDetailsService.createEntity(dto);
	}

	@PatchMapping(value = "/update")
	public ResponseEntity<String> updateEntityDetails(@RequestBody EntityRequestDTO dto) {
		log.info("Requesting for updateEntityDetails");
		return entityDetailsService.updateEntityDetails(dto);
	}

	@GetMapping(value = "/fetch/{entityId}")
	public ResponseEntity<EntityResponseDTO> fetchEntityDetails(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		log.info("Requesting fetchEntityDetails: {}", entityId);
		return entityDetailsService.fetchEntityDetails(entityId);
	}

	@GetMapping(value = "/fetchIndustryCategoryCode/{industryCategroyTypeCode}")
	public ResponseEntity<List<IndustryCategoryCode>> fetchIndustryCategoryCode(@PathVariable(value = "industryCategroyTypeCode", required = true) final String industryCategroyTypeCode) {
		log.info("Requesting for fetchIndustryCategoryCode");
		return companyDetailsService.fetchIndustryCategoryCode(industryCategroyTypeCode);
	}
	
	@PostMapping(value = "/saveIndustryDetails")
	public ResponseEntity<List<EntityIndustryClassification>> saveIndustryDetails(@RequestBody IndustryDetailsRequestDTO dto) {
		log.info("Requesting for saveIndustryDetails");
		companyDetailsService.saveIndustryDetails(dto);
		return companyDetailsService.fetchIndustryDetails(dto.getEntityId());
	}

	@PatchMapping(value = "/updateIndustryDetails")
	public ResponseEntity<List<EntityIndustryClassification>> updateIndustryDetails(@RequestBody IndustryDetailsRequestDTO dto) {
		log.info("Requesting for updateIndustryDetails");
		companyDetailsService.updateIndustryDetails(dto);
		return companyDetailsService.fetchIndustryDetails(dto.getEntityId());
	}

	@DeleteMapping(value = "/deleteIndustryDetailsByClassId/{entityIndustryClassId}")
	public ResponseEntity<String> deleteIndustryDetailsByClassId(@PathVariable(value = "entityIndustryClassId", required = true) final Integer entityIndustryClassId) {
		log.info("Requesting for deleteIndustryDetailsByClassId");
		return companyDetailsService.deleteIndustryDetailsByClassId(entityIndustryClassId);
	}

	@DeleteMapping(value = "/deleteIndustryDetailsByCatCode/{industryCatCode}")
	public ResponseEntity<String> deleteIndustryDetailsByCatCode(@PathVariable(value = "industryCatCode", required = true) final String industryCatCode) {
		log.info("Requesting for deleteIndustryDetailsByCatCode");
		return companyDetailsService.deleteIndustryDetailsByCatCode(industryCatCode);
	}

	@PostMapping(value = "/saveRegistrationDetails")
	public ResponseEntity<Map<String, Integer>> saveRegistrationDetails(@RequestBody RegistrationDetailsRequestDTO dto) {
		log.info("Requesting for saveRegistrationDetails");
		return companyDetailsService.saveRegistrationDetails(dto);
	}

	@PatchMapping(value = "/updateRegistrationDetails")
	public ResponseEntity<String> updateRegistrationDetails(@RequestBody RegistrationDetailsRequestDTO dto) {
		log.info("Requesting for updateRegistrationDetails");
		return companyDetailsService.updateRegistrationDetails(dto);
	}

	@DeleteMapping(value = "/deleteRegistrationDetails/{entityRegistrationId}")
	public ResponseEntity<String> deleteRegistrationDetails(@PathVariable(value = "entityRegistrationId", required = true) final Integer entityRegistrationId) {
		log.info("Requesting for deleteRegistrationDetails");
		return companyDetailsService.deleteRegistrationDetails(entityRegistrationId);
	}

	@PostMapping(value = "/saveAdditionalAddresses")
	public ResponseEntity<Map<String, Integer>> saveAdditionalAddresses(@RequestBody AddressDetailsRequestDTO dto) {
		log.info("Requesting for saveAdditionalAddresses");
		return companyDetailsService.saveAdditionalAddresses(dto);
	}

	@PatchMapping(value = "/updateAdditionalAddresses")
	public ResponseEntity<String> updateAdditionalAddresses(@RequestBody AddressDetailsRequestDTO dto) {
		log.info("Requesting for updateAdditionalAddresses");
		return companyDetailsService.updateAdditionalAddresses(dto);
	}

	@DeleteMapping(value = "/deleteAdditionalAddress/{entityMailingAddressId}")
	public ResponseEntity<String> deleteAdditionalAddress(@PathVariable(value = "entityMailingAddressId", required = true) final Integer entityMailingAddressId) {
		log.info("Requesting for deleteAdditionalAddress");
		return companyDetailsService.deleteAdditionalAddress(entityMailingAddressId);
	}

	@PatchMapping(value = "/updateOtherDetails")
	public ResponseEntity<String> updateOtherDetails(@RequestBody OtherDetailsRequestDTO dto) {
		log.info("Requesting for updateOtherDetails");
		return companyDetailsService.updateOtherDetails(dto);
	}

	@PostMapping(value = "/saveRisk")
	public ResponseEntity<Map<String, Integer>> saveRisk(@RequestBody EntityRiskRequestDTO dto) {
		log.info("Requesting for saveRisk");
		return entityRiskService.saveRisk(dto);
	}

	@PatchMapping(value = "/updateRisk")
	public ResponseEntity<String> updateRisk(@RequestBody EntityRiskRequestDTO dto) {
		log.info("Requesting for updateRisk");
		return entityRiskService.updateRisk(dto);
	}

	@DeleteMapping(value = "/deleteRisk/{entityRiskId}")
	public ResponseEntity<String> deleteRisk(@PathVariable(value = "entityRiskId", required = true) final Integer entityRiskId) {
		log.info("Requesting for deleteRisk");
		return entityRiskService.deleteRisk(entityRiskId);
	}

	@PostMapping(value = "/saveExternalReference")
	public ResponseEntity<Map<String, Integer>> saveExternalReference(@RequestBody ExternalReferenceRequestDTO dto) {
		log.info("Requesting for saveExternalReference");
		return entityExternalReferenceService.saveExternalReference(dto);
	}

	@PatchMapping(value = "/updateExternalReference")
	public ResponseEntity<String> updateExternalReference(@RequestBody ExternalReferenceRequestDTO dto) {
		log.info("Requesting for updateExternalReference");
		return entityExternalReferenceService.updateExternalReference(dto);
	}

	@DeleteMapping(value = "/deleteExternalReference/{entityExternalMappingId}")
	public ResponseEntity<String> deleteExternalReference(@PathVariable(value = "entityExternalMappingId", required = true) final Integer entityExternalMappingId) {
		log.info("Requesting for deleteExternalReference");
		return entityExternalReferenceService.deleteExternalReference(entityExternalMappingId);
	}

	@PostMapping(value = "/dunsNumberExists")
	public ResponseEntity<Boolean> dunsNumberExists(@RequestBody EntityRequestDTO dto) {
		log.info("Requesting for dunsNumberExists");
		return globalEntityService.isDunsNumberExists(dto.getDunsNumber());
	}

	@PostMapping(value = "/ueiNumberExists")
	public ResponseEntity<Boolean> ueiNumberExists(@RequestBody EntityRequestDTO dto) {
		log.info("Requesting for ueiNumberExists");
		return globalEntityService.isUeiNumberExists(dto.getUeiNumber());
	}

	@PostMapping(value = "/cageNumberExists")
	public ResponseEntity<Boolean> cageNumberExists(@RequestBody EntityRequestDTO dto) {
		log.info("Requesting for cageNumberExists");
		return globalEntityService.isCageNumberExists(dto.getCageNumber());
	}

	@GetMapping(value = "/fetchCurrencyDetails")
	public ResponseEntity<List<Currency>> fetchCurrencyDetails() {
		log.info("Requesting for fetchCurrencyDetails");
		return globalEntityService.fetchCurrencyDetails();
	}

	@PostMapping(value = "/addPriorName")
	public ResponseEntity<Map<String, Integer>> addPriorName(@RequestBody PriorNameRequestDTO dto) {
		log.info("Requesting for addPriorName");
		return companyDetailsService.addPriorName(dto);
	}

	@GetMapping(value = "/fetchPriorNames/{entityId}")
	public List<PriorNameResponseDTO> fetchPriorNames(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		log.info("Requesting for fetchPriorNames");
		return companyDetailsService.fetchPriorNames(entityId);
	}

	@DeleteMapping(value = "/deletePriorName/{id}")
	public ResponseEntity<String> deletePriorName(@PathVariable(value = "id", required = true) final Integer id) {
		log.info("Requesting for deletePriorName");
		return companyDetailsService.deletePriorName(id);
	}

	@PostMapping(value = "/addForeignName")
	public ResponseEntity<Map<String, Integer>> addForeignName(@RequestBody ForeignNameRequestDTO dto) {
		log.info("Requesting for addForeignName");
		return companyDetailsService.addForeignName(dto);
	}

	@GetMapping(value = "/fetchForeignNames/{entityId}")
	public List<ForeignNameResponseDTO> fetchForeignNames(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		log.info("Requesting for fetchForeignNames");
		return companyDetailsService.fetchForeignNames(entityId);
	}

	@DeleteMapping(value = "/deleteForeignName/{id}")
	public ResponseEntity<String> deleteForeignName(@PathVariable(value = "id", required = true) final Integer id) {
		log.info("Requesting for deleteForeignName");
		return companyDetailsService.deleteForeignName(id);
	}

	@GetMapping(value = "/fetchRiskTypes/{riskCategoryCode}")
	public ResponseEntity<List<EntityRiskType>> fetchRiskTypes(@PathVariable(value = "riskCategoryCode", required = true) final String riskCategoryCode) {
		log.info("Requesting for fetchRiskTypes");
		return entityRiskService.fetchRiskTypes(riskCategoryCode);
	}

	@GetMapping(value = "/fetchRiskLevels/{riskTypeCode}")
	public ResponseEntity<List<EntityRiskLevel>> fetchRiskLevels(@PathVariable(value = "riskTypeCode", required = true) final String riskTypeCode) {
		log.info("Requesting for fetchRiskTypes");
		return entityRiskService.fetchRiskLevels(riskTypeCode);
	}

	@PatchMapping(value = "/verify/{entityId}")
	public ResponseEntity<Map<String, Object>> verifyEntityDetails(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		log.info("Requesting for verifyEntityDetails");
		return globalEntityService.verifyEntityDetails(entityId);
	}

	@GetMapping(value = "/fetchEntityTabStatus/{entityId}")
	public Map<String, Object> fetchEntityTabStatus(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		log.info("Requesting for fetchEntityDetails");
		return globalEntityService.fetchEntityTabStatus(entityId);
	}

	@PostMapping(value = "/validateDuplicate")
	public ResponseEntity<List<validateDuplicateResponseDTO>> validateDuplicate(@RequestBody ValidateDuplicateRequestDTO dto) {
		log.info("Requesting for validateDuplicate");
		return new ResponseEntity<>(globalEntityService.validateDuplicate(dto), HttpStatus.OK);
	}

	@PostMapping(value = "/markDuplicate")
	public ResponseEntity<ResponseMessageDTO> markDuplicate(@RequestBody MarkDuplicateRequestDTO dto) {
		log.info("Requesting for markDuplicate");
		return new ResponseEntity<>(globalEntityService.markDuplicate(dto), HttpStatus.OK);
	}

	@GetMapping(value = "/fetchHistory/{entityId}")
	public List<EntityActionLogDto> fetchHistory(@PathVariable(value = "entityId", required = true) final Integer entityId) {
		log.info("Requesting for fetchHistory");
		return globalEntityService.fetchHistory(entityId);
	}

	@PostMapping(value = "/logAction")
	public ResponseEntity<ResponseMessageDTO> logAction(@RequestBody ActionLogRequestDTO dto) {
		log.info("Requesting for logAction");
		return new ResponseEntity<>(globalEntityService.logAction(dto), HttpStatus.OK);
	}

	@GetMapping(value = "/fetchRiskHistory/{entityRiskId}")
	public List<EntityRiskActionLogResponseDTO> fetchRiskHistory(@PathVariable(value = "entityRiskId", required = true) final Integer entityRiskId) {
		log.info("Requesting for fetchRiskHistory with id: {}", entityRiskId);
		return globalEntityService.fetchRiskHistory(entityRiskId);
	}

}
