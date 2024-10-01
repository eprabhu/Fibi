package com.polus.fibicomp.globalentity.service;

import java.util.List;
import java.util.Map;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

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

@Service
public interface GlobalEntityService {

	public default ResponseEntity<Map<String, Integer>> createEntity(EntityRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<String> updateEntityDetails(EntityRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<EntityResponseDTO> fetchEntityDetails(Integer entityId) {
		return null;
	}

	public default void saveIndustryDetails(IndustryDetailsRequestDTO dto) {
	}

	public default ResponseEntity<List<EntityIndustryClassification>> fetchIndustryDetails(Integer entityId) {
		return null;
	}

	public default ResponseEntity<String> updateIndustryDetails(IndustryDetailsRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<Map<String, Integer>> saveRegistrationDetails(RegistrationDetailsRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<String> updateRegistrationDetails(RegistrationDetailsRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<Map<String, Integer>> saveAdditionalAddresses(AddressDetailsRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<String> updateAdditionalAddresses(AddressDetailsRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<String> updateOtherDetails(OtherDetailsRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<String> updateRisk(EntityRiskRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<Map<String, Integer>> saveRisk(EntityRiskRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<Map<String, Integer>> saveExternalReference(ExternalReferenceRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<String> updateExternalReference(ExternalReferenceRequestDTO dto) {
		return null;
	}

	public default ResponseEntity<Object> fetchEntityOverview(Integer entityId) {
		return null;
	}

	public default ResponseEntity<Boolean> isDunsNumberExists(String dunsNumber) {
		return null;
	}

	public default ResponseEntity<Boolean> isUeiNumberExists(String ueiNumber) {
		return null;
	}

	public default ResponseEntity<Boolean> isCageNumberExists(String cageNumber) {
		return null;
	}

	public default ResponseEntity<String> deleteIndustryDetailsByClassId(Integer entityIndustryClassId) {
		return null;
	}

	public default ResponseEntity<String> deleteRegistrationDetails(Integer entityRegistrationId) {
		return null;
	}

	public default ResponseEntity<String> deleteAdditionalAddress(Integer entityMailingAddressId) {
		return null;
	}

	public default ResponseEntity<String> deleteRisk(Integer entityRiskId) {
		return null;
	}

	public default ResponseEntity<List<IndustryCategoryCode>> fetchIndustryCategoryCode(
			String industryCategroyTypeCode) {
		return null;
	}

	public default ResponseEntity<String> deleteExternalReference(Integer entityExternalMappingId) {
		return null;
	}

	public default ResponseEntity<List<Currency>> fetchCurrencyDetails() {
		return null;
	}

	public default ResponseEntity<Map<String, Integer>> addPriorName(PriorNameRequestDTO dto) {
		return null;
	}

	public default List<PriorNameResponseDTO> fetchPriorNames(Integer entityId) {
		return null;
	}

	public default ResponseEntity<Map<String, Integer>> addForeignName(ForeignNameRequestDTO dto) {
		return null;
	}

	public default List<ForeignNameResponseDTO> fetchForeignNames(Integer entityId) {
		return null;
	}

	public default ResponseEntity<String> deletePriorName(Integer id) {
		return null;
	}

	public default ResponseEntity<String> deleteForeignName(Integer id) {
		return null;
	}

	public default ResponseEntity<List<EntityRiskType>> fetchRiskTypes(String riskCategoryCode){
		return null;
	}

	public default ResponseEntity<Map<String, Object>> verifyEntityDetails(Integer entityId) {
		return null;
	}

	public default void processEntityMessageToQ(Integer entityId) {
	}

	public default ResponseEntity<String> deleteIndustryDetailsByCatCode(String industryCatCode) {
		return null;
	}

	public default ResponseEntity<List<EntityRiskLevel>> fetchRiskLevels(String riskTypeCode) {
		return null;
	}

	public default Map<String, Object> fetchEntityTabStatus(Integer entityId) {
		return null;
	}

	public default List<validateDuplicateResponseDTO> validateDuplicate(ValidateDuplicateRequestDTO dto) {
		return null;
	}

	public default ResponseMessageDTO markDuplicate(MarkDuplicateRequestDTO dto) {
		return null;
	}

	public default List<EntityActionLogDto> fetchHistory(Integer entityId) {
		return null;
	}

	public default ResponseMessageDTO logAction(ActionLogRequestDTO dto) {
		return null;
	}

	public default List<EntityRiskActionLogResponseDTO> fetchRiskHistory(Integer entityRiskId) {
		return null;
	}

}
