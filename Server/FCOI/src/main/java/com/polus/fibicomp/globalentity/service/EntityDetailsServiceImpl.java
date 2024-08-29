package com.polus.fibicomp.globalentity.service;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.globalentity.dao.EntityDetailsDAO;
import com.polus.fibicomp.globalentity.dao.EntityRiskDAO;
import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityResponseDTO;
import com.polus.fibicomp.globalentity.dto.ForeignNameResponseDTO;
import com.polus.fibicomp.globalentity.dto.PriorNameResponseDTO;
import com.polus.fibicomp.globalentity.pojo.Entity;
import com.polus.fibicomp.globalentity.pojo.EntityAttachment;
import com.polus.fibicomp.globalentity.pojo.EntityExternalIdMapping;
import com.polus.fibicomp.globalentity.pojo.EntityIndustryClassification;
import com.polus.fibicomp.globalentity.pojo.EntityMailingAddress;
import com.polus.fibicomp.globalentity.pojo.EntityRegistration;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;
import com.polus.fibicomp.globalentity.repository.EntityExternalIdMappingRepository;
import com.polus.fibicomp.globalentity.repository.EntityIndustryClassificationRepository;
import com.polus.fibicomp.globalentity.repository.EntityMailingAddressRepository;
import com.polus.fibicomp.globalentity.repository.EntityRegistrationRepository;
import com.polus.fibicomp.globalentity.repository.GlobalEntityRepository;

@Service(value = "entityDetailsService")
@Transactional
public class EntityDetailsServiceImpl implements EntityDetailsService {

	@Autowired
	private EntityDetailsDAO entityDetailsDAO;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CompanyDetailsService companyDetailsService;

	@Autowired
	private EntityIndustryClassificationRepository entityIndustryClassificationRepository;

	@Autowired
	private EntityRegistrationRepository entityRegistrationRepository;

	@Autowired
	private EntityMailingAddressRepository entityMailingAddressRepository;

	@Autowired
	private EntityExternalIdMappingRepository externalIdMappingRepository;

	@Autowired
	private GlobalEntityRepository entityRepository;

	@Autowired
	private EntityRiskDAO entityRiskDAO;

	@Autowired
	private EntityFileAttachmentService entityFileAttachmentService;

	private static final String GENERAL_SECTION_CODE = "1";

	@Override
	public ResponseEntity<Map<String, Integer>> createEntity(EntityRequestDTO dto) {
		Entity entity = mapDTOToEntity(dto);
		return new ResponseEntity<>(Map.of("entityId", entityDetailsDAO.createEntity(entity)), HttpStatus.OK);
	}

	private Entity mapDTOToEntity(EntityRequestDTO dto) {
		return Entity.builder().entityName(dto.getEntityName()).phoneNumber(dto.getPhoneNumber())
				.entityOwnershipTypeCode(dto.getEntityOwnershipTypeCode())
				.primaryAddressLine1(dto.getPrimaryAddressLine1()).primaryAddressLine2(dto.getPrimaryAddressLine2())
				.city(dto.getCity()).state(dto.getState()).postCode(dto.getPostCode()).countryCode(dto.getCountryCode())
				.certifiedEmail(dto.getCertifiedEmail()).websiteAddress(dto.getWebsiteAddress())
				.dunsNumber(dto.getDunsNumber()).ueiNumber(dto.getUeiNumber()).cageNumber(dto.getCageNumber()).entityStatusTypeCode("2")
				.updatedBy(AuthenticatedUser.getLoginPersonId()).updateTimestamp(commonDao.getCurrentTimestamp())
				.createdBy(AuthenticatedUser.getLoginPersonId()).createTimestamp(commonDao.getCurrentTimestamp())
				.versionNumber(1).versionStatus("ACTIVE").isActive(Boolean.TRUE)
				.build();
	}

	@Override
	public ResponseEntity<String> updateEntityDetails(EntityRequestDTO dto) {
		entityDetailsDAO.updateEntity(dto);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Entity updated successfully"), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<EntityResponseDTO> fetchEntityDetails(Integer entityId) {
		List<EntityIndustryClassification> entityIndustryClassifications = entityIndustryClassificationRepository
				.findByEntityId(entityId);
		List<EntityRegistration> entityRegistrations = entityRegistrationRepository.findByEntityId(entityId);
		List<EntityMailingAddress> entityMailingAddresses = entityMailingAddressRepository.findByEntityId(entityId);
		List<EntityRisk> entityRisks = entityRiskDAO.findEntityRiskByEntityId(entityId);
		List<EntityExternalIdMapping> EntityExternalIdMappings = externalIdMappingRepository.findByEntityId(entityId);
		List<PriorNameResponseDTO> priorNames = companyDetailsService.fetchPriorNames(entityId);
		List<ForeignNameResponseDTO> foreignNames = companyDetailsService.fetchForeignNames(entityId);
		List<EntityAttachment> attachments = entityFileAttachmentService.getAttachmentsBySectionCode(GENERAL_SECTION_CODE, entityId);
		Entity entityDetails = entityRepository.findByEntityId(entityId);
		return new ResponseEntity<>(EntityResponseDTO.builder().entityDetails(entityDetails)
				.entityIndustryClassifications(entityIndustryClassifications)
				.entityMailingAddresses(entityMailingAddresses).entityRegistrations(entityRegistrations)
				.entityRisks(entityRisks).entityExternalIdMappings(EntityExternalIdMappings).priorNames(priorNames)
				.foreignNames(foreignNames).attachments(attachments).build(), HttpStatus.OK);
	}

}
