package com.polus.fibicomp.globalentity.service;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.fibicomp.globalentity.dao.EntityRiskDAO;
import com.polus.fibicomp.globalentity.dao.SubAwdOrgDAO;
import com.polus.fibicomp.globalentity.dto.ActionLogRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityAttachmentResponseDTO;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgDetailsResponseDTO;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgRequestDTO;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgResponseDTO;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;
import com.polus.fibicomp.globalentity.pojo.EntitySubOrgInfo;
import com.polus.fibicomp.globalentity.repository.EntityFeedStatusTypeRepository;
import com.polus.fibicomp.globalentity.repository.EntitySubOrgInfoRepository;

@Service(value = "subAwardOrganizationService")
@Transactional
public class SubAwdOrgDetailsServiceImpl implements SubAwdOrgDetailsService {

	@Autowired
	private EntitySubOrgInfoRepository entitySubOrgInfoRepository;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private SubAwdOrgDAO subAwdOrgDAO;

	@Autowired
	private EntityRiskDAO entityRiskDAO;

	@Autowired
	private EntityFileAttachmentService entityFileAttachmentService;

	@Autowired
    private EntityActionLogService actionLogService;

	@Autowired
    private EntityFeedStatusTypeRepository feedStatusRepository;

	private static final String ORGANIZATION_SECTION_CODE = "3";
	private static final String FEED_STATUS_READY_TO_FEED = "2";
	private static final String FEED_STATUS_NOT_READY_TO_FEED = "1";
	private static final String ORGANIZATION_FEED_ACTION_LOG_CODE = "11";

	@Override
	public ResponseEntity<Map<String, Integer>> saveDetails(SubAwdOrgRequestDTO dto) {
		EntitySubOrgInfo entity = mapDTOToEntity(dto);
		return new ResponseEntity<>(Map.of("id", subAwdOrgDAO.saveDetails(entity)), HttpStatus.OK);
	}

	private EntitySubOrgInfo mapDTOToEntity(SubAwdOrgRequestDTO dto) {
		return EntitySubOrgInfo.builder().entityId(dto.getEntityId()).samExpirationDate(dto.getSamExpirationDate())
				.subAwdRiskAssmtDate(dto.getSubAwdRiskAssmtDate()).organizationId(dto.getOrganizationId())
				.organizationTypeCode(dto.getOrganizationTypeCode()).feedStatusCode(dto.getFeedStatusCode()).build();
	}

	@Override
	public ResponseEntity<String> updateDetails(SubAwdOrgRequestDTO dto) {
		if (dto.getFeedStatusCode() != null) {
			EntitySubOrgInfo orgInfo = entitySubOrgInfoRepository.findByEntityId(dto.getEntityId());
			if (orgInfo.getEntityFeedStatusType() == null
					|| !feedStatusRepository.getDescriptionByCode(FEED_STATUS_READY_TO_FEED).equals(orgInfo.getEntityFeedStatusType().getDescription())) {
				ActionLogRequestDTO logDTO = ActionLogRequestDTO.builder().entityId(dto.getEntityId())
						.oldFeedStatus(orgInfo.getEntityFeedStatusType() == null
								? feedStatusRepository.getDescriptionByCode(FEED_STATUS_NOT_READY_TO_FEED)
								: orgInfo.getEntityFeedStatusType().getDescription())
						.newFeedStatus(feedStatusRepository.getDescriptionByCode(FEED_STATUS_READY_TO_FEED)).build();
				actionLogService.saveEntityActionLog(ORGANIZATION_FEED_ACTION_LOG_CODE, logDTO, null);
			}
		}
		subAwdOrgDAO.updateDetails(dto);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Sub Award Organization details updated successfully"), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<SubAwdOrgResponseDTO> fetchDetails(Integer entityId) {
		SubAwdOrgDetailsResponseDTO subAwdOrgDetailsResponseDTOs = mapEntityToDTO(entitySubOrgInfoRepository.findByEntityId(entityId));
		List<EntityRisk> entityRisks = entityRiskDAO.findSubAwdOrgRiskByEntityId(entityId);
		List<EntityAttachmentResponseDTO> attachments = entityFileAttachmentService.getAttachmentsBySectionCode(ORGANIZATION_SECTION_CODE, entityId);
		return new ResponseEntity<>(
				SubAwdOrgResponseDTO.builder().subAwdOrgDetailsResponseDTO(subAwdOrgDetailsResponseDTOs).entityRisks(entityRisks).attachments(attachments)
				.build(),
				HttpStatus.OK);
	}

	private SubAwdOrgDetailsResponseDTO mapEntityToDTO(EntitySubOrgInfo entitySubOrgInfos) {
		if (entitySubOrgInfos != null) {
			return SubAwdOrgDetailsResponseDTO.builder().entityId(entitySubOrgInfos.getEntityId())
					.id(entitySubOrgInfos.getId())
					.organizationId(entitySubOrgInfos.getOrganizationId())
					.entityOrganizationType(entitySubOrgInfos.getEntityOrganizationType())
					.samExpirationDate(entitySubOrgInfos.getSamExpirationDate())
					.subAwdRiskAssmtDate(entitySubOrgInfos.getSubAwdRiskAssmtDate()).build();
		} else {
			return null;
		}
	}

	@Override
	public ResponseEntity<String> deleteDetails(Integer id) {
		entitySubOrgInfoRepository.deleteByEntitySubOrgInfoId(id);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Sub Award Organization details deleted successfully"), HttpStatus.OK);
	}

}
