package com.polus.fibicomp.manpowerintegration.service;

import java.io.IOException;
import java.sql.Timestamp;
import java.text.ParseException;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.springframework.oxm.XmlMappingException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;
import com.polus.fibicomp.manpowerintegration.pojo.AwardManpowerBaseSalaryHistory;
import com.polus.fibicomp.manpowerintegration.vo.ManpowerIntegrationVO;


@Transactional
@Service
public interface ManpowerIntegrationService {

	public String createWorkdayPosition(AwardManpowerResource awardManpowerResource, String pISuprgId, Map<String, String> workdayConfigDetails, Integer awardId, Integer workdayManpowerInterfaceId) throws XmlMappingException, IOException;

	public void getWorkdayLongLeave(AtomicInteger rowCount) throws ParseException;

	public void getWorkdayTerminations(AtomicInteger rowCount) throws ParseException;

	public String checkAndCreateSupervisoryOrganizationId(Award award, Map<String, String> workdayConfigDetails, WorkdayManpowerInterface workdayManpowerInterface);

	public void createPosition(WorkdayManpowerInterface workdaymanpowerInterface, Map<String, String> workdayConfigDetails,  String superiorSupOrg, String piPersonId, Integer activeAwardId);

	public String getCostingAllocationReconciliation(Entry<String, List<AwardManpowerResource>> awardManpowerResources, Map<String, String> workdayConfigDetails, AtomicBoolean success, AtomicBoolean isWorkday, AtomicBoolean isRaise, StringBuilder mailContent, AtomicInteger rowCount, Set<String> uniqueId, Set<String> successUniqueId, List<AwardManpowerResource> costAllocationSuccessResources) throws ParseException;

	public void freezePosition(WorkdayManpowerInterface workdaymanpowerInterface, Map<String, String> workdayConfigDetails, Award award, StringBuilder freezeErrorMailContent);

	public void getWorkdayJobProfile(AtomicInteger rowCount);

	public Boolean getWorkdayHRBusinessPartner(AwardManpowerResource resource, String supOrgId, Map<String, String> workdayConfigDetails, Integer awardId, Integer workdayManpowerInterfaceId, Integer activeAwardId);

	public boolean checkIfAwardHasManpowerStaffResource(Integer awardId);

	public void sendHrbpDetailsToPI(WorkdayManpowerInterface workdayManpowerInterface, Map<String, String> workdayConfigDetails, String superiorSupOrg, String piPersonId, Integer activeAwardId);

	public void assignCostAllocation(WorkdayManpowerInterface workdayManpowerInterface, Map<String, String> workdayConfigDetails, List<AwardManpowerResource> costAllocationSuccessResources, Integer activeAwardId);

	public void moveWorkers(WorkdayManpowerInterface workdayManpowerInterface, Map<String, String> workdayConfigDetails, Integer activeAwardId, StringBuilder moveWorkerSuccessMailContent, StringBuilder moveWorkerErrorMailContent);

	/**
	 * This method is used to get the manpower details
	 * @param rowCount 
	 * @throws ParseException 
	 */
	public void getManpowerDetails(AtomicInteger rowCount) throws ParseException;

	/**
	 * This method is used to get the nationality details
	 * @param rowCount 
	 * @param rowCount 
	 * @throws ParseException 
	 */
	public void getNationalityDetails(AtomicInteger rowCount) throws ParseException;

	/**
	 * This method is used to prepate date time based on differance
	 * @param toDateTime
	 * @param fromDateTime
	 * @param dayDifference
	 * @return updates timestamp
	 */
	public Timestamp prepareFromDateTime(Timestamp toDateTime, Timestamp fromDateTime, String dayDifference);

	/**
	 * This method is used to close workday position
	 * @param postionId
	 * @param awardNumber
	 * @param awardId 
	 * @param workdayApi 
	 * @return True if success else false
	 * @throws XmlMappingException
	 * @throws IOException
	 */
	public Boolean closeWorkdayPosition(String postionId, String awardNumber, StringBuilder errorMessage, Map<String, String> workdayConfigDetails, Integer awardId) throws XmlMappingException, IOException;

	public void prepareAndSendFreezeReducedMail(String positionIds, Award award);

	public void prepareAndSendAllocationLessThanHundredMail(List<WorkdayManpowerInterface> allocationLessThanHundredInterfaces, StringBuilder allocationContent,Integer awardId);

	public void getManPowerDetails();

	public Map<String, String> getWorkdayConfigDetails();

	public void getJobProfileChanges();

	public void saveManpowerLog(String awardNumber, String interfaceTypeCode, String messageType, String message, int statusCode, String request, String errorXPath, String errorDetailMessage, Integer awardId,  Integer workdayManpowerIntefaceId);

	public void saveWorkdayManpowerInterfaceWithInterfaceStatus(WorkdayManpowerInterface workdayManpowerInterface, String interfaceStatusCode);

	public void saveBaseSalaryHistory(AwardManpowerBaseSalaryHistory baseSalaryHistory, AwardManpowerResource awardManpowerResource, Timestamp chargeStartDate, Timestamp chargeEndDate);

	/**
	 * This method is used to fetch manpower lookup last synced details
	 * @return
	 */
	public String getManpowerLookupSyncDetails();

	/**
	 * This method is used to fetch award trigger details
	 * @param ManpowerIntegrationVO vo
	 */
	public String getAwardTriggerDetails(ManpowerIntegrationVO vo);

	/**
	 * This method is used to fetch position trigger details
	 * @param ManpowerIntegrationVO vo
	 */
	public String getPositionTriggerDetails(ManpowerIntegrationVO vo);

	/**
	 * This method is used to fetch Manpower position Errors trigger details
	 * @param ManpowerIntegrationVO vo
	 */
	public String getPositionErrorDetails(ManpowerIntegrationVO vo);

	/**
	 * This method is used to send notification based on department
	 * @return 
	 */
	public void sendNotificationForLessCostAllocation();

	/**
	 * This method is used to fetch cost allocation trigger details
	 * @param ManpowerIntegrationVO vo
	 */
	public String getCostAllocationTriggerDetails(ManpowerIntegrationVO vo);

	/**
	 * This method is used to fetch current cost allocation trigger details of a person
	 * @param ManpowerIntegrationVO vo
	 */
	public String getCurrentCostAllocationDetails(ManpowerIntegrationVO vo);

	/**
	 * This method is used to update workday manpower interface manually
	 * @param ManpowerIntegrationVO vo
	 */
	public String updateManpowerInterfaceManually(ManpowerIntegrationVO vo);

	/**
	 * This method is used to update workday manpower interface
	 * @param workdayManpowerInterface -workdayManpowerInterface
	 * @param mailFlag -mailFlag
	 */
	public void saveWorkdayManpowerInterfaceWithMailFlag(WorkdayManpowerInterface workdayManpowerInterface, String mailFlag);

	/**
	 * This method is used to send notification for PI change or Superior Change(Move Worker Event)
	 * @param oldPiPersonId
	 * @param newPiPersonId
	 * @param oldSuperiorSupOrgId
	 * @param newSuperiorSupOrgId
	 * @param awardId
	 */
	public void prepareNotificationForChangeInPIOrSuperiorSupOrgId(String oldPiPersonId, String newPiPersonId, String oldSuperiorSupOrgId, String newSuperiorSupOrgId, Integer awardId);

	/**
	 * This method is used to send notification for PI change or Superior Change(Move Worker Event)
	 * @param exclusionMailContent
	 * @param successMailContent
	 * @param errorMailContent
	 * @param awardId
	 */
	public void sendMoveWorkerNotificationMail(StringBuilder exclusionMailContent, StringBuilder successMailContent, StringBuilder errorMailContent, Integer awardId);

}
