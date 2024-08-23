package com.polus.fibicomp.globalentity.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.globalentity.dto.ComplianceResponseDTO;

@Service
public interface ComplianceService {

	public default ResponseEntity<ComplianceResponseDTO> fetchDetails(Integer entityId) {
		return null;
	}

}
