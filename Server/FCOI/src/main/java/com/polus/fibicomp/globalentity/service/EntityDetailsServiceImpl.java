package com.polus.fibicomp.globalentity.service;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.globalentity.dao.EntityDetailsDAO;
import com.polus.fibicomp.globalentity.dao.EntityRiskDAO;
import com.polus.fibicomp.globalentity.dto.ActionLogRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityAttachmentResponseDTO;
import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityRequestField;
import com.polus.fibicomp.globalentity.dto.EntityResponseDTO;
import com.polus.fibicomp.globalentity.dto.ForeignNameResponseDTO;
import com.polus.fibicomp.globalentity.dto.PriorNameResponseDTO;
import com.polus.fibicomp.globalentity.pojo.Entity;
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

import lombok.extern.slf4j.Slf4j;

@Service(value = "entityDetailsService")
@Transactional
@Slf4j
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

	@Autowired
    private EntityActionLogService actionLogService;

	protected static Logger logger = LogManager.getLogger(EntityDetailsServiceImpl.class.getName());
	private static final String GENERAL_SECTION_CODE = "1";
	private static final String DOCUMENT_STATUS_ACTIVE = "1";
	private static final String CREATE_ACTION_LOG_CODE = "1";
	private static final String ENTITY_STATUS_TYPE_CODE = "2";
	private static final Integer VERSION_NUMBER = 1;
	private static final String VERSION_STATUS = "ACTIVE";

	@Override
	public ResponseEntity<Map<String, Integer>> createEntity(EntityRequestDTO dto) {
		Entity entity = null;
		Integer entityId = null;

		try {
			entity = mapDTOToEntity(dto);

			if (entity == null) {
				logger.error("Entity mapping failed for DTO: {}", dto);
				return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
			}

			entityId = entityDetailsDAO.createEntity(entity);

			ActionLogRequestDTO logDTO = ActionLogRequestDTO.builder()
					.entityId(entityId)
					.entityName(entity.getEntityName())
					.updatedBy(entity.getUpdatedBy())
					.build();
			actionLogService.saveEntityActionLog(CREATE_ACTION_LOG_CODE, logDTO, null);
			logger.info("Entity created successfully with ID: {}", entityId);

		} catch (Exception e) {
			logger.error("Error in createEntity: {}", e.getMessage(), e);
			return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
		}

		return new ResponseEntity<>(Map.of("entityId", entityId), HttpStatus.OK);
	}

	private Entity mapDTOToEntity(EntityRequestDTO dto) {
		if (dto == null || dto.getEntityRequestFields() == null || dto.getEntityRequestFields().isEmpty()) {
			logger.error("Invalid EntityRequestDTO: DTO or fields are null/empty");
			return null;
		}

		try {
			Map<EntityRequestField, Object> entityRequestFields = dto.getEntityRequestFields();
			Entity.EntityBuilder entityBuilder = Entity.builder()
					.entityStatusTypeCode(ENTITY_STATUS_TYPE_CODE)
					.updatedBy(AuthenticatedUser.getLoginPersonId())
					.createdBy(AuthenticatedUser.getLoginPersonId())
					.updateTimestamp(commonDao.getCurrentTimestamp())
					.createTimestamp(commonDao.getCurrentTimestamp())
					.documentStatusTypeCode(DOCUMENT_STATUS_ACTIVE)
					.versionNumber(VERSION_NUMBER)
					.versionStatus(VERSION_STATUS)
					.isActive(Boolean.TRUE);

			entityRequestFields.forEach((field, value) -> mapFieldToEntity(entityBuilder, field, value));

			return entityBuilder.build();

		} catch (Exception e) {
			logger.error("Error mapping DTO to Entity: {}", e.getMessage(), e);
			return null;
		}
	}

	private void mapFieldToEntity(Entity.EntityBuilder entityBuilder, EntityRequestField field, Object value) {
		switch (field) {
			case entityName:
				entityBuilder.entityName(castToString(value));
				break;
			case phoneNumber:
				entityBuilder.phoneNumber(castToString(value));
				break;
			case entityOwnershipTypeCode:
				entityBuilder.entityOwnershipTypeCode(castToString(value));
				break;
			case primaryAddressLine1:
				entityBuilder.primaryAddressLine1(castToString(value));
				break;
			case primaryAddressLine2:
				entityBuilder.primaryAddressLine2(castToString(value));
				break;
			case city:
				entityBuilder.city(castToString(value));
				break;
			case state:
				entityBuilder.state(castToString(value));
				break;
			case postCode:
				entityBuilder.postCode(castToString(value));
				break;
			case countryCode:
				entityBuilder.countryCode(castToString(value));
				break;
			case certifiedEmail:
				entityBuilder.certifiedEmail(castToString(value));
				break;
			case websiteAddress:
				entityBuilder.websiteAddress(castToString(value));
				break;
			case dunsNumber:
				entityBuilder.dunsNumber(castToString(value));
				break;
			case ueiNumber:
				entityBuilder.ueiNumber(castToString(value));
				break;
			case cageNumber:
				entityBuilder.cageNumber(castToString(value));
				break;
			case animalAccreditation:
				entityBuilder.animalAccreditation(castToString(value));
				break;
			case anumalWelfareAssurance:
				entityBuilder.anumalWelfareAssurance(castToString(value));
				break;
			case approvedBy:
				entityBuilder.approvedBy(castToString(value));
				break;
			case approvedTimestamp:
				entityBuilder.approvedTimestamp(castToTimestamp(value));
				break;
			case documentStatusTypeCode:
				entityBuilder.documentStatusTypeCode(castToString(value));
				break;
			case entityNumber:
				entityBuilder.entityNumber(castToInteger(value));
				break;
			case entityStatusTypeCode:
				entityBuilder.entityStatusTypeCode(castToString(value));
				break;
			case humanSubAssurance:
				entityBuilder.isDunsMatched(castToBoolean(value));
				break;
			case isDunsMatched:
				entityBuilder.websiteAddress(castToString(value));
				break;
			case originalEntityId:
				entityBuilder.originalEntityId(castToInteger(value));
				break;
			default:
				logger.warn("Unhandled EntityRequestField: {}", field);
				break;
		}
	}

	private String castToString(Object value) {
		 return value instanceof String && !((String) value).isEmpty() ? (String) value : null;
	}

	private Integer castToInteger(Object value) {
		return value instanceof Integer ? (Integer) value : null;
	}

	private Timestamp castToTimestamp(Object value) {
		return value instanceof Timestamp ? (Timestamp) value : null;
	}

	private Boolean castToBoolean(Object value) {
		return value instanceof Boolean ? (Boolean) value : null;
	}

	@Override
	public ResponseEntity<String> updateEntityDetails(EntityRequestDTO dto) {
		if (dto.getCountryCode() != null && dto.getCountryCode().length() == 2) {
			dto.setCountryCode(commonDao.fetchCountryByCountryTwoCode(dto.getCountryCode()).getCountryCode());
		}
		entityDetailsDAO.updateEntity(dto);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Entity updated successfully"), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<EntityResponseDTO> fetchEntityDetails(Integer entityId) {
		String originalName = null;
		Integer originalEntityId;
		List<EntityIndustryClassification> entityIndustryClassifications = entityIndustryClassificationRepository
				.findByEntityId(entityId);
		List<EntityRegistration> entityRegistrations = entityRegistrationRepository.findByEntityId(entityId);
		List<EntityMailingAddress> entityMailingAddresses = entityMailingAddressRepository.findByEntityId(entityId);
		List<EntityRisk> entityRisks = entityRiskDAO.findEntityRiskByEntityId(entityId);
		List<EntityExternalIdMapping> EntityExternalIdMappings = externalIdMappingRepository.findByEntityId(entityId);
		List<PriorNameResponseDTO> priorNames = companyDetailsService.fetchPriorNames(entityId);
		List<ForeignNameResponseDTO> foreignNames = companyDetailsService.fetchForeignNames(entityId);
		List<EntityAttachmentResponseDTO> attachments = entityFileAttachmentService.getAttachmentsBySectionCode(GENERAL_SECTION_CODE, entityId);
		Entity entityDetails = entityRepository.findByEntityId(entityId);
		originalEntityId = entityDetails.getOriginalEntityId();
		log.info("originalEntityId : {}", originalEntityId);
		if (originalEntityId != null) {
			originalName = entityRepository.fetchEntityNameByEntityId(originalEntityId);
			log.info("originalName : {}", originalName);
		}
		Map<String, Object> entityTabStatus = entityDetailsDAO.getEntityTabStatus(entityId);
		return new ResponseEntity<>(EntityResponseDTO.builder().entityDetails(entityDetails)
				.entityIndustryClassifications(entityIndustryClassifications)
				.entityMailingAddresses(entityMailingAddresses).entityRegistrations(entityRegistrations)
				.entityRisks(entityRisks).entityExternalIdMappings(EntityExternalIdMappings).priorNames(priorNames)
				.foreignNames(foreignNames).attachments(attachments).entityTabStatus(entityTabStatus).originalName(originalName)
				.build(), HttpStatus.OK);
	}

}
