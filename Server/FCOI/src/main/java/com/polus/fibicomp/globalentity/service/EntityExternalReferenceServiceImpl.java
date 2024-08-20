package com.polus.fibicomp.globalentity.service;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.globalentity.dao.EntityExternalReferenceDAO;
import com.polus.fibicomp.globalentity.dto.ExternalReferenceRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntityExternalIdMapping;
import com.polus.fibicomp.globalentity.repository.EntityExternalIdMappingRepository;

@Service(value = "entityExternalReferenceService")
@Transactional
public class EntityExternalReferenceServiceImpl implements EntityExternalReferenceService {

	@Autowired
	private EntityExternalReferenceDAO dao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private EntityExternalIdMappingRepository entityExternalIdMappingRepository;

	@Override
	public ResponseEntity<Map<String, Integer>> saveExternalReference(ExternalReferenceRequestDTO dto) {
		EntityExternalIdMapping entity = mapDTOToEntity(dto);
		return new ResponseEntity<>(Map.of("entityExternalMappingId", dao.saveEntityExternalReference(entity)),
				HttpStatus.OK);
	}

	private EntityExternalIdMapping mapDTOToEntity(ExternalReferenceRequestDTO dto) {
		return EntityExternalIdMapping.builder().entityId(dto.getEntityId()).organizationId(dto.getOrganizationId())
				.sponsorCode(dto.getSponsorCode()).updatedBy(AuthenticatedUser.getLoginPersonId())
				.updateTimestamp(commonDao.getCurrentTimestamp()).build();
	}

	@Override
	public ResponseEntity<String> updateExternalReference(ExternalReferenceRequestDTO dto) {
		dao.updatExternalReference(dto);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Entity external reference updated successfully"), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<String> deleteExternalReference(Integer entityExternalMappingId) {
		entityExternalIdMappingRepository.deleteByEntityExternalMappingId(entityExternalMappingId);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Entity external reference deleted successfully"), HttpStatus.OK);
	}
	
}
