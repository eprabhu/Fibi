package com.polus.integration.award.service;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.award.dao.AwardIntegrationDao;
import com.polus.integration.award.dto.AwardDTO;
import com.polus.integration.award.dto.AwardPersonDTO;
import com.polus.integration.award.pojo.COIIntegrationAward;
import com.polus.integration.award.pojo.COIIntegrationAwardPerson;
import com.polus.integration.award.repository.AwardPersonRepository;
import com.polus.integration.award.repository.AwardRepository;
import com.polus.integration.client.FcoiFeignClient;
import com.polus.integration.constant.Constant;
import com.polus.integration.dao.IntegrationDao;
import com.polus.integration.exception.service.MQRouterException;
import com.polus.integration.instituteProposal.repository.InstituteProposalRepository;
import com.polus.integration.instituteProposal.vo.DisclosureSyncVO;

import feign.FeignException;
import lombok.extern.slf4j.Slf4j;

@Transactional
@Service
@Slf4j
public class AwardIntegrationServiceImpl implements AwardIntegrationService {

	@Autowired
	private AwardIntegrationDao awardDao;

	@Autowired
	private InstituteProposalRepository proposalRepository;

	@Autowired
	private AwardPersonRepository  projectPersonRepository;

	@Autowired
	private AwardRepository awardRepository;

	@Autowired
	private FcoiFeignClient fcoiFeignClient;

	@Autowired
	private IntegrationDao integrationDao;

	@Value("${fibi.messageq.queues.awardIntegration}")
	private String awardIntegrationQueue;
 
	@Override
	public ResponseEntity<AwardDTO> feedAward(AwardDTO awardDTO) {
		try {
			COIIntegrationAward award = awardRepository.findProjectByProjectNumber(awardDTO.getProjectNumber());
			Boolean canUpdateProjectDisclosureFlag = awardDao.canUpdateProjectDisclosureFlag(awardDTO);
			if (award != null) {
				saveOrUpdateCOIAward(awardDTO, award);
			} else {
				COIIntegrationAward coiIntAward = new COIIntegrationAward();
				coiIntAward.setFirstFedTimestamp(integrationDao.getCurrentTimestamp());
				saveOrUpdateCOIAward(awardDTO, coiIntAward);
			}
			prepareProjectPersonDetail(awardDTO);
			if (Boolean.TRUE.equals(canUpdateProjectDisclosureFlag)) {
				updateDisclSyncFlag(awardDTO.getProjectNumber());
			}
			linkOrUnlinkIPFromAward(awardDTO.getLinkedInstProposalNumbers(), awardDTO.getProjectNumber());
		} catch (Exception e) {
	        log.error("General exception occurred in feedInstituteProposal for project: {}", awardDTO.getProjectNumber(), e.getMessage());
	        throw new MQRouterException(Constant.ERROR_CODE, "Exception in feed award integration", e, e.getMessage(),
	        		awardIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE,
	                Constant.AWARD_MODULE_CODE, Constant.SUB_MODULE_CODE,
	                Constant.AWARD_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
	    }
		postIntegrationProcess(awardDTO.getProjectNumber());
	    return new ResponseEntity<>(awardDTO, HttpStatus.OK);
	}

	private void postIntegrationProcess(String projectNumber) {
		awardRepository.COI_SYNC_REMOVE_DEACTIVATED_PROJECTS(Constant.AWARD_MODULE_CODE, projectNumber);
	}

	private void prepareProjectPersonDetail(AwardDTO awardDTO) {
		try {
			List<COIIntegrationAwardPerson> projectPersons = projectPersonRepository.findProjectPersonsByProjectNumber(awardDTO.getProjectNumber());
			awardDTO.getProjectPersons().forEach(projectPersonDTO -> {
				COIIntegrationAwardPerson projectPerson = projectPersons.stream()
	                    .filter(person -> person.getKeyPersonId().equals(projectPersonDTO.getKeyPersonId())
	                            && person.getProjectNumber().equals(projectPersonDTO.getProjectNumber()))
	                    .findFirst()
	                    .orElse(new COIIntegrationAwardPerson());
	            prepareProjectPersonDetail(projectPerson, projectPersonDTO);
	            projectPersonRepository.save(projectPerson);
	        });

	        // Mark as inactive if any existing person is removed 
	        Set<String> incomingKeyPersonIds = awardDTO.getProjectPersons().stream().map(projectPersonDTO -> projectPersonDTO.getKeyPersonId())
	                .collect(Collectors.toSet());
	        projectPersons.stream().filter(existingPerson -> !incomingKeyPersonIds.contains(existingPerson.getKeyPersonId()))
	                .forEach(existingPerson -> {
	                    existingPerson.setStatus(Constant.INACTIVE);
	                    projectPersonRepository.save(existingPerson);
	                });

	    } catch (DataAccessException e) {
	        log.error("Database exception occurred while preparing project person details for project: {}", awardDTO.getProjectNumber(), e.getMessage());
	        throw new MQRouterException(Constant.ERROR_CODE, "Database exception in preparing project person details", e, e.getMessage(),
	        		awardIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE,
	                Constant.AWARD_MODULE_CODE, Constant.SUB_MODULE_CODE,
	                Constant.AWARD_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
	    }
	}

	private void prepareProjectPersonDetail(COIIntegrationAwardPerson projectPerson, AwardPersonDTO projectPersonDTO) {
		if (projectPerson.getProjectNumber() == null) {
			projectPerson.setProjectNumber(projectPersonDTO.getProjectNumber());
			projectPerson.setKeyPersonId(projectPersonDTO.getKeyPersonId());
		}
		projectPerson.setKeyPersonRoleCode(projectPersonDTO.getKeyPersonRoleCode());
        projectPerson.setKeyPersonRoleName(projectPersonDTO.getKeyPersonRoleName());
		projectPerson.setAttribute1Label(projectPersonDTO.getAttribute1Label());
        projectPerson.setAttribute1Value(projectPersonDTO.getAttribute1Value());
        projectPerson.setAttribute2Label(projectPersonDTO.getAttribute2Label());
        projectPerson.setAttribute2Value(projectPersonDTO.getAttribute2Value());
        projectPerson.setAttribute3Label(projectPersonDTO.getAttribute3Label());
        projectPerson.setAttribute3Value(projectPersonDTO.getAttribute3Value());
        projectPerson.setKeyPersonName(projectPersonDTO.getKeyPersonName());
        projectPerson.setStatus(Constant.ACTIVE);
        projectPerson.setPercentOfEffort(projectPersonDTO.getPercentOfEffort());
        projectPersonRepository.save(projectPerson);
	}

	private void saveOrUpdateCOIAward(AwardDTO awardDTO, COIIntegrationAward award) {
		award.setProjectStatus(awardDTO.getProjectStatus());
		award.setProjectStatusCode(awardDTO.getProjectStatusCode());
		award.setProjectNumber(awardDTO.getProjectNumber());
		award.setProjectId(awardDTO.getProjectId());
		award.setTitle(awardDTO.getTitle());
		award.setVersionNumber(awardDTO.getVersionNumber());
		award.setRootProjectNumber(awardDTO.getRootProjectNumber());
		award.setParentProjectNumber(awardDTO.getParentProjectNumber());
		award.setAccountNumber(awardDTO.getAccountNumber());
		award.setAnticipatedTotal(awardDTO.getAnticipatedTotal());
		award.setObligatedTotal(awardDTO.getObligatedTotal());
		award.setDocumentUrl(awardDTO.getDocumentUrl());
		award.setLastFedTimestamp(integrationDao.getCurrentTimestamp());
		award.setLeadUnitName(awardDTO.getLeadUnitName());
		award.setLeadUnitNumber(awardDTO.getLeadUnitNumber());
		award.setPrimeSponsorCode(awardDTO.getPrimeSponsorCode());
		award.setPrimeSponsorName(awardDTO.getPrimeSponsorName());
		award.setProjectStartDate(awardDTO.getProjectStartDate());
		award.setProjectEndDate(awardDTO.getProjectEndDate());
		award.setProjectType(awardDTO.getProjectType());
		award.setProjectTypeCode(awardDTO.getProjectTypeCode());
		award.setSponsorCode(awardDTO.getSponsorCode());
		award.setSponsorName(awardDTO.getSponsorName());
		award.setSponsorGrantNumber(awardDTO.getSponsorGrantNumber());
		award.setSrcSysUpdatedBy(awardDTO.getSrcSysUpdatedBy());
		award.setSrcSysUpdateTimestamp(awardDTO.getSrcSysUpdateTimestamp());
		award.setAttribute1Label(awardDTO.getAttribute1Label());
		award.setAttribute1Value(awardDTO.getAttribute1Value());
		award.setAttribute2Label(awardDTO.getAttribute2Label());
		award.setAttribute2Value(awardDTO.getAttribute2Value());
		award.setAttribute3Label(awardDTO.getAttribute3Label());
		award.setAttribute3Value(awardDTO.getAttribute3Value());
		award.setAttribute4Label(awardDTO.getAttribute4Label());
		award.setAttribute4Value(awardDTO.getAttribute4Value());
		award.setAttribute5Label(awardDTO.getAttribute5Label());
		award.setAttribute5Value(awardDTO.getAttribute5Value());
		awardRepository.save(award);
	}

	private void updateDisclSyncFlag(String projectNumber) {
		try {
			DisclosureSyncVO vo = new DisclosureSyncVO();
			vo.setModuleCode(Constant.AWARD_MODULE_CODE);
			vo.setProjectId(projectNumber);
			ResponseEntity<Object> response = fcoiFeignClient.updateProjectDisclosureFlag(vo);
			if (response.getStatusCode() == HttpStatus.INTERNAL_SERVER_ERROR || response.getStatusCode() == HttpStatus.BAD_REQUEST
					|| response.getStatusCode() == HttpStatus.UNAUTHORIZED || response.getStatusCode() == HttpStatus.NOT_FOUND) {
				log.error("Exception occurred in disclosure syncNeeded API. Project number : {}", projectNumber);
				return; // Return immediately if the status code is 500 or 400
			} else if (response.getStatusCode() == HttpStatus.OK) {
				log.info("Dislosure Sync flag updated successfully");
			}
	    } catch (FeignException e) {
	        log.error("Feign client exception occurred during disclosure sync for project: {}", projectNumber, e.getMessage());
	        throw new MQRouterException(Constant.ERROR_CODE, "Feign client exception in disclosure sync", e, e.getMessage(),
	        		awardIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE,
	                Constant.AWARD_MODULE_CODE, Constant.SUB_MODULE_CODE,
	                Constant.AWARD_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
	    } catch (DataAccessException e) {
	        log.error("Database exception occurred during disclosure sync for project: {}", projectNumber, e.getMessage());
	        throw new MQRouterException(Constant.ERROR_CODE, "Database exception in disclosure sync", e, e.getMessage(),
	        		awardIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE,
	                Constant.AWARD_MODULE_CODE, Constant.SUB_MODULE_CODE,
	                Constant.AWARD_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
	    }

	}

	private void linkOrUnlinkIPFromAward(List<String> linkedIPNumbers, String projectNumber) {
		try {
			if (linkedIPNumbers.isEmpty()) {
				proposalRepository.unlinkAwardFromAllIPs(projectNumber);
			} else {
				List<String> ipNumbers = proposalRepository.findIPNumbersByAwardNumber(projectNumber);
				Set<String> linkedIPs = new HashSet<>(linkedIPNumbers);
				ipNumbers.forEach(ipNumber -> {
					if (linkedIPs.contains(ipNumber)) {
						proposalRepository.linkAwardNumberInIP(projectNumber, ipNumber);
						linkedIPs.remove(ipNumber);
					} else {
						proposalRepository.unlinkAwardNumberFromIP(projectNumber, ipNumber);
					}
				});
				linkedIPs.forEach(ipNumber -> proposalRepository.linkAwardNumberInIP(projectNumber, ipNumber));
			}
	    } catch (DataAccessException e) {
	        log.error("Database exception occurred while linking/unlinking IP numbers in development proposals for project: {}", projectNumber, e.getMessage());
	        throw new MQRouterException(Constant.ERROR_CODE, "Database exception in linking/unlinking IP numbers", e, e.getMessage(),
	        		awardIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE,
	                Constant.AWARD_MODULE_CODE, Constant.SUB_MODULE_CODE,
	                Constant.AWARD_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
	    }
	}

}
