package com.polus.fibicomp.globalentity.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.globalentity.dao.EntityRiskDAO;
import com.polus.fibicomp.globalentity.dto.ComplianceResponseDTO;
import com.polus.fibicomp.globalentity.dto.EntityAttachmentResponseDTO;
import com.polus.fibicomp.globalentity.pojo.EntityRisk;

@Service
@Transactional
public class ComplianceServiceImpl implements ComplianceService {

	@Autowired
	private EntityRiskDAO entityRiskDAO;

	@Autowired
	private EntityFileAttachmentService entityFileAttachmentService;

	private static final String COMPLAINCE_SECTION_CODE = "4";

	@Override
	public ResponseEntity<ComplianceResponseDTO> fetchDetails(Integer entityId) {
		List<EntityRisk> entityRisks = entityRiskDAO.findComplianceRiskByEntityId(entityId);
		List<EntityAttachmentResponseDTO> attachments = entityFileAttachmentService.getAttachmentsBySectionCode(COMPLAINCE_SECTION_CODE, entityId);
		return new ResponseEntity<>(ComplianceResponseDTO.builder().entityRisks(entityRisks).attachments(attachments)
				.build(), HttpStatus.OK);
	}

}
