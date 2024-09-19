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
import com.polus.fibicomp.globalentity.dto.SubAwdOrgDetailsResponseDTO;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgRequestDTO;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgResponseDTO;
import com.polus.fibicomp.globalentity.pojo.EntityAttachment;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;
import com.polus.fibicomp.globalentity.pojo.EntitySubOrgInfo;
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

	private static final String ORGANIZATION_SECTION_CODE = "3";

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
		subAwdOrgDAO.updateDetails(dto);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Sub Award Organization details updated successfully"), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<SubAwdOrgResponseDTO> fetchDetails(Integer entityId) {
		SubAwdOrgDetailsResponseDTO subAwdOrgDetailsResponseDTOs = mapEntityToDTO(entitySubOrgInfoRepository.findByEntityId(entityId));
		List<EntityRisk> entityRisks = entityRiskDAO.findSubAwdOrgRiskByEntityId(entityId);
		List<EntityAttachment> attachments = entityFileAttachmentService.getAttachmentsBySectionCode(ORGANIZATION_SECTION_CODE, entityId);
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
