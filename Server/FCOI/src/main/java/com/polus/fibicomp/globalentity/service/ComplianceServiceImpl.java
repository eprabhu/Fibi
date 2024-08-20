package com.polus.fibicomp.globalentity.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.globalentity.dao.EntityRiskDAO;
import com.polus.fibicomp.globalentity.dto.ComplianceResponseDTO;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;

@Service
@Transactional
public class ComplianceServiceImpl implements ComplianceService {

	@Autowired
	private EntityRiskDAO entityRiskDAO;

	@Override
	public ResponseEntity<ComplianceResponseDTO> fetchDetails(Integer entityId) {
		List<EntityRisk> entityRisks = entityRiskDAO.findComplianceRiskByEntityId(entityId);
		return new ResponseEntity<>(ComplianceResponseDTO.builder().entityRisks(entityRisks).build(), HttpStatus.OK);
	}

}
