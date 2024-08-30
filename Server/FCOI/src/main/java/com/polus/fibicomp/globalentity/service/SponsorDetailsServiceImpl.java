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
import com.polus.fibicomp.globalentity.dto.SponsorDetailsResponseDTO;
import com.polus.fibicomp.globalentity.dto.SponsorRequestDTO;
import com.polus.fibicomp.globalentity.dto.SponsorResponseDTO;
import com.polus.fibicomp.globalentity.pojo.EntityAttachment;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;
import com.polus.fibicomp.globalentity.pojo.EntitySponsorInfo;
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

	private static final String SPONSOR_SECTION_CODE = "2";

	@Override
	public ResponseEntity<Map<String, Integer>> saveDetails(SponsorRequestDTO dto) {
		EntitySponsorInfo entity = mapDTOToEntity(dto);
		return new ResponseEntity<>(Map.of("id", sponsorDAO.saveDetails(entity)), HttpStatus.OK);
	}

	private EntitySponsorInfo mapDTOToEntity(SponsorRequestDTO dto) {
		return EntitySponsorInfo.builder().entityId(dto.getEntityId()).acronym(dto.getAcronym()).sponsorTypeCode(dto.getSponsorTypeCode()).build();
	}

	@Override
	public ResponseEntity<String> updateDetails(SponsorRequestDTO dto) {
		sponsorDAO.updateDetails(dto);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Sponsor details updated successfully"), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<SponsorResponseDTO> fetchDetails(Integer entityId) {
		SponsorDetailsResponseDTO sponsorDetailsResponseDTO = mapEntityToDTO(entitySponsorInfoRepository.findByEntityId(entityId));
		List<EntityRisk> entityRisks = entityRiskDAO.findSponsorRiskByEntityId(entityId);
		List<EntityAttachment> attachments = entityFileAttachmentService.getAttachmentsBySectionCode(SPONSOR_SECTION_CODE, entityId);
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
					.sponsorTypeCode(entitySponsorInfo.getSponsorTypeCode()).build();
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
