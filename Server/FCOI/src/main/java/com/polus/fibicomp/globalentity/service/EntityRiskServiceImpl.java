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
import com.polus.fibicomp.globalentity.dao.EntityRiskDAO;
import com.polus.fibicomp.globalentity.dto.EntityRiskRequestDTO;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;
import com.polus.fibicomp.globalentity.pojo.EntityRiskType;
import com.polus.fibicomp.globalentity.repository.EntityRiskRepository;
import com.polus.fibicomp.globalentity.repository.EntityRiskTypeRepository;

@Service(value = "entityRiskService")
@Transactional
public class EntityRiskServiceImpl implements EntityRiskService {

	@Autowired
	private EntityRiskDAO entityRiskDAO;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private EntityRiskRepository entityRiskRepository;

	@Autowired
	private EntityRiskTypeRepository entityRiskTypeRepository;

	@Override
	public ResponseEntity<Map<String, Integer>> saveRisk(EntityRiskRequestDTO dto) {
		EntityRisk entity = mapDTOToEntity(dto);
		return new ResponseEntity<>(Map.of("entityRiskId", entityRiskDAO.saveEntityRisk(entity)), HttpStatus.OK);
	}

	private EntityRisk mapDTOToEntity(EntityRiskRequestDTO dto) {
		return EntityRisk.builder().entityId(dto.getEntityId()).riskTypeCode(dto.getRiskTypeCode())
				.riskLevelCode(dto.getRiskLevelCode()).description(dto.getDescription())
				.updatedBy(AuthenticatedUser.getLoginPersonId()).updateTimestamp(commonDao.getCurrentTimestamp())
				.build();
	}

	@Override
	public ResponseEntity<String> updateRisk(EntityRiskRequestDTO dto) {
		entityRiskDAO.updateEntityRisk(dto);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Entity risk updated successfully"), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<String> deleteRisk(Integer entityRiskId) {
		entityRiskRepository.deleteByEntityRiskId(entityRiskId);
		return new ResponseEntity<>(commonDao.convertObjectToJSON("Entity risk deleted successfully"), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<List<EntityRiskType>> fetchRiskTypes() {
		return new ResponseEntity<>(entityRiskTypeRepository.fetchRiskTypes(), HttpStatus.OK);
	}

}
