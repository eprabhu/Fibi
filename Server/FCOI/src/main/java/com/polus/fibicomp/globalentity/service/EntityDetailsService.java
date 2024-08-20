package com.polus.fibicomp.globalentity.service;

import java.util.Map;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.globalentity.dto.EntityRequestDTO;
import com.polus.fibicomp.globalentity.dto.EntityResponseDTO;

@Service
public interface EntityDetailsService extends GlobalEntityService {

	ResponseEntity<Map<String, Integer>> createEntity(EntityRequestDTO dto);

	ResponseEntity<String> updateEntityDetails(EntityRequestDTO dto);

	ResponseEntity<EntityResponseDTO> fetchEntityDetails(Integer entityId);

}
