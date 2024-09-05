package com.polus.fibicomp.globalentity.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.messageq.config.MessageQServiceRouter;
import com.polus.core.messageq.vo.MessageQVO;
import com.polus.core.messageq.vo.MessagingQueueProperties;
import com.polus.core.pojo.Currency;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.globalentity.dao.EntityDetailsDAO;
import com.polus.fibicomp.globalentity.dao.SponsorDAO;
import com.polus.fibicomp.globalentity.dao.SubAwdOrgDAO;
import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.dto.SponsorRequestDTO;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgRequestDTO;
import com.polus.fibicomp.globalentity.dto.ValidateDuplicateRequestDTO;
import com.polus.fibicomp.globalentity.dto.validateDuplicateResponseDTO;
import com.polus.fibicomp.globalentity.pojo.Entity;
import com.polus.fibicomp.globalentity.pojo.EntitySponsorInfo;
import com.polus.fibicomp.globalentity.pojo.EntitySubOrgInfo;
import com.polus.fibicomp.globalentity.repository.EntitySponsorInfoRepository;
import com.polus.fibicomp.globalentity.repository.EntitySubOrgInfoRepository;
import com.polus.fibicomp.globalentity.repository.GlobalEntityRepository;

@Service(value = "globalEntityService")
@Transactional
public class GlobalEntityServiceImpl implements GlobalEntityService {

	@Autowired
	@Lazy
	private GlobalEntityRepository entityRepository;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private EntityDetailsDAO entityDetailsDAO;

	@Autowired
	private SubAwdOrgDAO subAwdOrgDAO;

	@Autowired
	private SponsorDAO sponsorDAO;

	@Autowired
	private MessagingQueueProperties messagingQueueProperties;

	@Autowired
	private MessageQServiceRouter messageQServiceRouter;

	@Autowired
	private EntitySponsorInfoRepository entitySponsorInfoRepository;

	@Autowired
	private EntitySubOrgInfoRepository entitySubOrgInfoRepository;

	private static final Integer ENTITY_MODULE_CODE = 26;

	@Override
	public ResponseEntity<Boolean> isDunsNumberExists(String dunsNumber) {
		return new ResponseEntity<>(entityRepository.isDunsNumberExists(dunsNumber) > 0, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Boolean> isCageNumberExists(String cageNumber) {
		return new ResponseEntity<>(entityRepository.isCageNumberExists(cageNumber) > 0, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Boolean> isUeiNumberExists(String ueiNumber) {
		return new ResponseEntity<>(entityRepository.isUeiNumberExists(ueiNumber) > 0, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<List<Currency>> fetchCurrencyDetails(){
		return new ResponseEntity<>(commonDao.fetchCurrencyDetails(), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Map<String, Object>> verifyEntityDetails(Integer entityId) {
		Map<String, Object> entityTabStatus = entityDetailsDAO.getEntityTabStatus(entityId);
		Set<String> requiredKeys = Set.of("entity_sponsor_info", "entity_sub_org_info");
		boolean allRequiredTabsComplete = requiredKeys.stream()
				.allMatch(key -> Boolean.TRUE.equals(entityTabStatus.get(key)));
		entityDetailsDAO.updateEntity(
				EntityRequestDTO.builder().entityId(entityId).approvedBy(AuthenticatedUser.getLoginPersonId())
						.approvedTimestamp(commonDao.getCurrentTimestamp()).entityStatusTypeCode("1").build());
		if (allRequiredTabsComplete) {
			sponsorDAO.updateDetails(SponsorRequestDTO.builder().entityId(entityId).feedStatusCode("2").build());
			subAwdOrgDAO.updateDetails(SubAwdOrgRequestDTO.builder().entityId(entityId).feedStatusCode("2").build());
		}
		processEntityMessageToQ(null, entityId, null, null);
		return new ResponseEntity<>(entityDetailsDAO.getEntityTabStatus(entityId), HttpStatus.OK);
	}

	public void processEntityMessageToQ(String actionType, Integer moduleItemKey, Integer moduleSubItemKey, Map<String, String> additionDetails) {
        MessageQVO messageQVO = new MessageQVO();
        messageQVO.setActionType(actionType);
        messageQVO.setModuleCode(ENTITY_MODULE_CODE);
        messageQVO.setSubModuleCode(null);
        messageQVO.setPublishedUserName(AuthenticatedUser.getLoginPersonId());
        messageQVO.setPublishedTimestamp(commonDao.getCurrentTimestamp());
        messageQVO.setOrginalModuleItemKey(moduleItemKey);
        messageQVO.setSubModuleItemKey(moduleSubItemKey);
        messageQVO.setSourceExchange(messagingQueueProperties.getQueues().get("exchange"));
        messageQVO.setSourceQueueName(messagingQueueProperties.getQueues().get("entity.integration"));
        messageQVO.setAdditionalDetails(additionDetails);
        messageQServiceRouter.getMessagingQueueServiceBean().publishMessageToQueue(messageQVO);
    }

	@Override
	public Map<String, Object> fetchEntityTabStatus(Integer entityId) {
		return entityDetailsDAO.getEntityTabStatus(entityId);
	}

	@Override
	public List<validateDuplicateResponseDTO> validateDuplicate(ValidateDuplicateRequestDTO dto) {
		List<Entity> entities = new ArrayList<Entity>();
		entities = entityDetailsDAO.validateDuplicateByParams(dto);
		List<validateDuplicateResponseDTO> responseDto = entities.stream()
			    .map(entity -> mapToResponseDto(entity))
			    .collect(Collectors.toList());
		return responseDto;
	}

	private validateDuplicateResponseDTO mapToResponseDto(Entity entity) {
		EntitySponsorInfo sponsorDetails = entitySponsorInfoRepository.findByEntityId(entity.getEntityId());
		EntitySubOrgInfo organizationDetails = entitySubOrgInfoRepository.findByEntityId(entity.getEntityId());
		return validateDuplicateResponseDTO.builder().entityId(entity.getEntityId()).entityName(entity.getEntityName())
				.primaryAddressLine1(entity.getPrimaryAddressLine1())
				.primaryAddressLine2(entity.getPrimaryAddressLine2()).country(entity.getCountry().getCountryName())
				.city(entity.getCity()).state(entity.getState()).dunsNumber(entity.getDunsNumber())
				.ueiNumber(entity.getUeiNumber()).cageNumber(entity.getCageNumber()).website(entity.getWebsiteAddress())
				.email(entity.getCertifiedEmail())
				.sponsorCode(sponsorDetails != null ? sponsorDetails.getSponsorCode() : null)
				.organizationId(organizationDetails != null ? organizationDetails.getOrganizationId() : null).build();
	}

}
