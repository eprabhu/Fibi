package com.polus.fibicomp.coi.hierarchy.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.hierarchy.dao.HierarchyDao;
import com.polus.fibicomp.coi.hierarchy.dto.HierarchyProjResponseDto;
import com.polus.fibicomp.coi.hierarchy.dto.HierarchyResponseDto;
import com.polus.fibicomp.coi.hierarchy.dto.ProjectHierarchyDto;
import com.polus.fibicomp.coi.hierarchy.dto.ProjectPersonDto;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fcoiDisclosure.pojo.CoiProjectType;

@Service
@Transactional
public class HierarchyService {
	
	@Autowired
	private HierarchyDao hierarchyDao;

	@Autowired
	private ConflictOfInterestDao conflictOfInterestDao;

	public ResponseEntity<Object> fetchProjectTree(Integer moduleCode, String projectNumber) {
	    List<ProjectHierarchyDto> hierarchyData = hierarchyDao.fetchProjectTree(moduleCode, projectNumber);
	    Map<String, HierarchyResponseDto> treeRoot = new HashMap<>();
	    prepareProjectTree(treeRoot, hierarchyData);
	    HierarchyResponseDto root = treeRoot.values().stream().findFirst().orElse(new HierarchyResponseDto());
	    return new ResponseEntity<>(root, HttpStatus.OK);
	}

	private void prepareProjectTree(Map<String, HierarchyResponseDto> rootMap, List<ProjectHierarchyDto> hierarchyData) {
		for (ProjectHierarchyDto data : hierarchyData) {
	        String awardNumber = data.getAwardNumber();
	        String ipNumber = data.getIpNumber();
	        String proposalNumber = data.getProposalNumber();
	        List<CoiProjectType> coiProjectTypes = conflictOfInterestDao.getCoiProjectTypes();
	        HierarchyResponseDto rootNode = handleRootNode(rootMap, awardNumber, Constants.AWARD_MODULE_CODE.toString(), coiProjectTypes);
	        if (rootNode != null) {
	            handleIpNode(rootNode, ipNumber, proposalNumber, coiProjectTypes);
	        } else {
	            // Handle IP Node when there's no award number
	            rootNode = handleRootNode(rootMap, ipNumber, Constants.INST_PROPOSAL_MODULE_CODE.toString(), coiProjectTypes);
	            if (rootNode != null) {
	                handleProposalNode(rootNode, proposalNumber, coiProjectTypes);
	            } else {
	                // Handle Proposal Node when there's no award or IP number
	                rootNode = handleRootNode(rootMap, proposalNumber, Constants.DEV_PROPOSAL_MODULE_CODE.toString(), coiProjectTypes);
	            }
	        }
	    }
	}

	private HierarchyResponseDto handleRootNode(Map<String, HierarchyResponseDto> rootMap, String projectNumber, String moduleCode, List<CoiProjectType> coiProjectTypes) {
		return (projectNumber != null) ? rootMap.computeIfAbsent(projectNumber, key -> {
	        HierarchyResponseDto dto = new HierarchyResponseDto();
	        dto.setProjectTypeCode(moduleCode.toString());
	        coiProjectTypes.parallelStream().filter(projectType -> moduleCode.equals(projectType.getCoiProjectTypeCode()))
	        .findFirst().ifPresent(projectType -> {
	        	dto.setProjectIcon(projectType.getProjectIcon());
        		dto.setProjectType(projectType.getDescription());
	        });
	        dto.setProjectNumber(projectNumber);
	        dto.setLinkedModule(new ArrayList<>());
	        return dto;
	    }) : null;
	}

	private void handleIpNode(HierarchyResponseDto rootNode, String ipNumber, String proposalNumber, List<CoiProjectType> coiProjectTypes) {
	    if (ipNumber != null) {
	        HierarchyResponseDto ipNode = rootNode.getLinkedModule().stream()
	                .filter(child -> child.getProjectNumber().equals(ipNumber))
	                .findFirst()
	                .orElseGet(() -> {
	                    HierarchyResponseDto child = new HierarchyResponseDto();
	                    child.setProjectTypeCode(Constants.INST_PROPOSAL_MODULE_CODE.toString());
	                    child.setProjectNumber(ipNumber);
	                    coiProjectTypes.parallelStream().filter(projectType -> Constants.INST_PROPOSAL_MODULE_CODE.toString().equals(projectType.getCoiProjectTypeCode()))
	        	        .findFirst().ifPresent(projectType -> {
	        	        	child.setProjectIcon(projectType.getProjectIcon());
	                		child.setProjectType(projectType.getDescription());
	        	        });
	                    child.setLinkedModule(new ArrayList<>());
	                    rootNode.getLinkedModule().add(child);
	                    return child;
	                });

	        if (proposalNumber != null) {
	            handleProposalNode(ipNode, proposalNumber, coiProjectTypes);
	        }
	    }
	}

	private void handleProposalNode(HierarchyResponseDto parentNode, String proposalNumber, List<CoiProjectType> coiProjectTypes) {
	    if (proposalNumber != null) {
	        HierarchyResponseDto proposalNode = new HierarchyResponseDto();
	        proposalNode.setProjectTypeCode(Constants.DEV_PROPOSAL_MODULE_CODE.toString());
	        proposalNode.setProjectNumber(proposalNumber);
	        coiProjectTypes.parallelStream().filter(projectType -> Constants.DEV_PROPOSAL_MODULE_CODE.toString().equals(projectType.getCoiProjectTypeCode()))
	        .findFirst().ifPresent(projectType -> {
	        	proposalNode.setProjectIcon(projectType.getProjectIcon());
	        	proposalNode.setProjectType(projectType.getDescription());
	        });
	        proposalNode.setLinkedModule(new ArrayList<>());
	        parentNode.getLinkedModule().add(proposalNode);
	    }
	}

	public ResponseEntity<Object> fetchProjectDetails(Integer moduleCode, String projectNumber) {
		HierarchyProjResponseDto response = hierarchyDao.fetchProjectDetails(moduleCode, projectNumber);
		List<ProjectPersonDto> projectPersons = response.getProjectPersons();
		projectPersons.stream().forEach(person -> {
			person.setDisclosures(hierarchyDao.getPersonDisclosures(moduleCode, projectNumber, person.getKeyPersonId()));
		});
		return new ResponseEntity<>(response, HttpStatus.OK);
	}


}
