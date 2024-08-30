package com.polus.fibicomp.globalentity.service;

import java.util.Map;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.globalentity.dto.SubAwdOrgRequestDTO;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgResponseDTO;

@Service
public interface SubAwdOrgDetailsService extends SubAwdOrgService{

	ResponseEntity<Map<String, Integer>> saveDetails(SubAwdOrgRequestDTO dto);

	ResponseEntity<String> updateDetails(SubAwdOrgRequestDTO dto);

	ResponseEntity<SubAwdOrgResponseDTO> fetchDetails(Integer entityId);

	ResponseEntity<String> deleteDetails(Integer id);

}
