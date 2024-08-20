package com.polus.fibicomp.globalentity.service;

import java.util.Map;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.globalentity.dto.SponsorRequestDTO;
import com.polus.fibicomp.globalentity.dto.SponsorResponseDTO;
import com.polus.fibicomp.globalentity.dto.SubAwdOrgResponseDTO;

@Service
public interface SponsorDetailsService extends SponosrService{

	ResponseEntity<Map<String, Integer>> saveDetails(SponsorRequestDTO dto);

	ResponseEntity<String> updateDetails(SponsorRequestDTO dto);

	ResponseEntity<SponsorResponseDTO> fetchDetails(Integer entityId);

	ResponseEntity<String> deleteDetails(Integer id);

}
