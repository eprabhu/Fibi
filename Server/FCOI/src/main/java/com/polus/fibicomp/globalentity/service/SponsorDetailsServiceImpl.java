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
import com.polus.fibicomp.globalentity.dao.SponsorDAO;
import com.polus.fibicomp.globalentity.dto.ActionLogRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityAttachmentResponseDTO;
import com.polus.fibicomp.globalentity.dto.SponsorDetailsResponseDTO;
import com.polus.fibicomp.globalentity.dto.SponsorRequestDTO;
import com.polus.fibicomp.globalentity.dto.SponsorResponseDTO;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;
import com.polus.fibicomp.globalentity.pojo.EntitySponsorInfo;
import com.polus.fibicomp.globalentity.repository.EntityFeedStatusTypeRepository;
import com.polus.fibicomp.globalentity.repository.EntitySponsorInfoRepository;

@Service
@Transactional
public class SponsorDetailsServiceImpl implements SponsorDetailsService {

	@Autowired
	private EntitySponsorInfoRepository entitySponsorInfoRepository;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private SponsorDAO sponsorDAO;

	@Autowired
	private EntityRiskDAO entityRiskDAO;

	@Autowired
	private EntityFileAttachmentService entityFileAttachmentService;

	@Autowired
    private EntityActionLogService actionLogService;

	@Autowired
    private EntityFeedStatusTypeRepository feedStatusRepository;

	private static final String SPONSOR_SECTION_CODE = "2";
	private static final String FEED_STATUS_READY_TO_FEED = "2";
	private static final String FEED_STATUS_NOT_READY_TO_FEED = "1";
	private static final String SPONSOR_FEED_ACTION_LOG_CODE = "10";
	private static final String ACTION_TYPE_SAVE = "S";
	private static final String ACTION_TYPE_UPDATE = "U";

	@Override
	public Map<String, Integer> saveDetails(SponsorRequestDTO dto) {
		EntitySponsorInfo entity = mapDTOToEntity(dto);
		return Map.of("id", sponsorDAO.saveDetails(entity));
	}

	private EntitySponsorInfo mapDTOToEntity(SponsorRequestDTO dto) {
		return EntitySponsorInfo.builder().entityId(dto.getEntityId()).acronym(dto.getAcronym()).sponsorTypeCode(dto.getSponsorTypeCode()).feedStatusCode(dto.getFeedStatusCode()).build();
	}

	@Override
	public ResponseEntity<String> updateDetails(SponsorRequestDTO dto) {
		dto.setAcType(ACTION_TYPE_UPDATE);
		logAction(dto);
		sponsorDAO.updateDetails(dto);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Sponsor details updated successfully"), HttpStatus.OK);
	}

	public void logAction(SponsorRequestDTO dto) {
		if (dto.getFeedStatusCode() != null) {
			if (dto.getAcType().equals(ACTION_TYPE_SAVE)) {
				ActionLogRequestDTO logDTO = ActionLogRequestDTO.builder().entityId(dto.getEntityId())
						.oldFeedStatus(feedStatusRepository.getDescriptionByCode(FEED_STATUS_NOT_READY_TO_FEED))
						.newFeedStatus(feedStatusRepository.getDescriptionByCode(FEED_STATUS_READY_TO_FEED)).build();
				actionLogService.saveEntityActionLog(SPONSOR_FEED_ACTION_LOG_CODE, logDTO, null);
			} else {
				EntitySponsorInfo sponsorInfo = entitySponsorInfoRepository.findByEntityId(dto.getEntityId());
				if (sponsorInfo.getEntityFeedStatusType() == null
						|| !feedStatusRepository.getDescriptionByCode(FEED_STATUS_READY_TO_FEED).equals(sponsorInfo.getEntityFeedStatusType().getDescription())) {
					ActionLogRequestDTO logDTO = ActionLogRequestDTO.builder().entityId(dto.getEntityId())
							.oldFeedStatus(sponsorInfo.getEntityFeedStatusType() == null
									? feedStatusRepository.getDescriptionByCode(FEED_STATUS_NOT_READY_TO_FEED)
									: sponsorInfo.getEntityFeedStatusType().getDescription())
							.newFeedStatus(feedStatusRepository.getDescriptionByCode(FEED_STATUS_READY_TO_FEED))
							.build();
					actionLogService.saveEntityActionLog(SPONSOR_FEED_ACTION_LOG_CODE, logDTO, null);
				}
			}
		}
	}

	@Override
	public ResponseEntity<SponsorResponseDTO> fetchDetails(Integer entityId) {
		SponsorDetailsResponseDTO sponsorDetailsResponseDTO = mapEntityToDTO(entitySponsorInfoRepository.findByEntityId(entityId));
		List<EntityRisk> entityRisks = entityRiskDAO.findSponsorRiskByEntityId(entityId);
		List<EntityAttachmentResponseDTO> attachments = entityFileAttachmentService.getAttachmentsBySectionCode(SPONSOR_SECTION_CODE, entityId);
		return new ResponseEntity<>(
				SponsorResponseDTO.builder().sponsorDetailsResponseDTO(sponsorDetailsResponseDTO).entityRisks(entityRisks).attachments(attachments)
				.build(),
				HttpStatus.OK);
	}

	private SponsorDetailsResponseDTO mapEntityToDTO(EntitySponsorInfo entitySponsorInfo) {
		if (entitySponsorInfo != null) {
			return SponsorDetailsResponseDTO.builder().entityId(entitySponsorInfo.getEntityId())
					.acronym(entitySponsorInfo.getAcronym()).id(entitySponsorInfo.getId())
					.sponsorCode(entitySponsorInfo.getSponsorCode())
					.sponsorType(entitySponsorInfo.getSponsorType()).build();
		} else {
			return null;
		}
	}

	@Override
	public ResponseEntity<String> deleteDetails(Integer id) {
		entitySponsorInfoRepository.deleteByEntitySponsorInfoId(id);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Sponsor details deleted successfully"), HttpStatus.OK);
	}

}
