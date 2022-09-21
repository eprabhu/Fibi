package com.polus.fibicomp.manpowerintegration.dao;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpower.pojo.ManpowerInterfaceStatus;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;
import com.polus.fibicomp.manpower.pojo.ManpowerTemp;
import com.polus.fibicomp.manpower.pojo.ManpowerUserAction;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;
import com.polus.fibicomp.manpowerintegration.dto.AwardClosePositionResorceDto;
import com.polus.fibicomp.manpowerintegration.dto.WorkdayInterfaceLogDto;
import com.polus.fibicomp.manpowerintegration.dto.terminations.TerminationsResourceResultDto;
import com.polus.fibicomp.manpowerintegration.pojo.AwardManpowerBaseSalaryHistory;
import com.polus.fibicomp.manpowerintegration.pojo.AwardSupOrgMapping;
import com.polus.fibicomp.manpowerintegration.pojo.ManpowerLog;
import com.polus.fibicomp.manpowerintegration.pojo.MigrationManpowerPerson;
import com.polus.fibicomp.manpowerintegration.pojo.RiseErrorAllocations;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayConfigurationData;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayJobProfileChange;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayLongLeaveDetails;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayTerminationDetails;
import com.polus.fibicomp.manpowerintegration.vo.AwardPersonVo;
import com.polus.fibicomp.manpowerintegration.vo.ManpowerIntegrationVO;

@Transactional
@Service
public interface ManpowerIntegrationDao {

	public List<WorkdayManpowerInterface> fetchManpowerInterfacesByInterfaceStatus(String interfaceStatus);

	public List<Award> getActiveAwardsByAwardPersonAndRoles(String personId, List<Integer> roleIds);

	public List<AwardManpowerResource> getAwardManpowerResourcesByPersonIdAndPositionStatus(String personId, String positionStatusCode);

	public String getManpowerConfigurationValue(String string);

	public List<TerminationsResourceResultDto> getTerminatedResourcesByPersonIdAndPositionStatusAndTerminationDate(String personId, String positionStatusCode, Timestamp ts);

	public String getPIPersonIdByAwardId(Integer awardId);

	public String getReferSubUnitFlagByUnitNumber(String unitNumber);

	public String getSuperiorSupervisoryOrganizationIdByUnitNumber(String awardLeadUnit);

	public List<WorkdayManpowerInterface> fetchManpowerInterfacesForCostAllocationAfterManpower(String interfaceStatus, String interfaceType);

	public AwardManpowerResource getAwardManpowerResourceById(Integer awardManpowerResourceId);

	public WorkdayManpowerInterface saveOrUpdateManpowerInterface(WorkdayManpowerInterface manpowerInterface);

	public void saveOrUpdateJobProfileType(ManpowerJobProfileType jobProfile);

	public boolean checkIfAwardHasManpowerStaffResource(Integer awardId);

	public void saveOrUpdateManpowerLog(ManpowerLog manpowerLog);

	public String getPiSupOrgByPiPersonIdAndSuperior(String pIPersonId, String superiorSupOrgId);

	public void saveOrUpdateAwardSupOrgMapping(AwardSupOrgMapping awardSupOrgMapping);

	public List<WorkdayManpowerInterface> fetchManpowerInterfacesByAwardNumberAndInterfaceStatusAndInterfaceTypeCodesAndResourceUniqueId(String awardNumber, String manpowerInterfaceStatus, List<String> interfaceTypecodes, String resourceUniqueId);

	public AwardSupOrgMapping getLatestAwardSupOrgByAwardNumber(String awardNumber);

	public void saveOrUpdateAwardManpowerResource(AwardManpowerResource awardManpowerResource);

	public List<AwardManpowerResource> getAwardManpowerResourcesByResourceUniqueId(String resourceUniqueId);

	public void saveOrUpdateWorkdayManpowerInterface(WorkdayManpowerInterface workdayManpowerInterface);

	/**
	 * This method is used to check whether Award PI Sup Org mappping exists
	 * @param awardNumber
	 * @param pIPersonId
	 * @param superiorSupOrgId
	 * @return boolean
	 */
	public boolean checkIfAwardPISUpOrgMappingExist(String awardNumber, String pIPersonId, String superiorSupOrgId);

	public AwardManpower getAwardManpowerById(Integer awardManpowerId);

	/**
	 * This method is used to get the Award PI Sup Org mappping
	 * @param awardNumber
	 * @param pIPersonId
	 * @param superiorSupOrgId
	 * @return list of AwardSupOrgMapping
	 */
	public List<AwardSupOrgMapping> getAwardUpOrgMappingByAwardNumberAndPIPersonId(String awardNumber, String pIPersonId, String superiorSupOrgId);

	public List<AwardManpowerResource> getOwnedAwardManpowerResourcesByAwardNumber(String awardNumber);

	/**
	 * This method is used to get the manpower by person id
	 * @param personId
	 * @return manpower
	 */
	public Manpower getManpowerByPersonId(String personId);

	/**
	 * This method is used to save the manpower
	 * @param manpower
	 */
	public void saveOrUpdate(Manpower manpower);

	/**
	 * This method is used to get manpower resource by position id
	 * @param positionId
	 * @return list of manpower resource
	 */
	public List<AwardManpowerResource> getManpowerResourceByPostionIdAndStatuses(String positionId, List<String> statuses);

	/**
	 * This method is used to fetch the award number by date and active
	 * @param yesterday
	 * @return list of award number
	 */
	public List<String> fetchAwardNumberByActive(Timestamp yesterday);

	/**
	 * This method is used to get the position ids by award numbers
	 * @param awardNumbers
	 * @return list of award manpower resources
	 */
	public List<AwardManpowerResource> getPostionIdsByAwardNumber(List<String> awardNumbers);

	/**
	 * This method is used to fetch the manpower resource where actual amount is null
	 * @return list of award manpower resources
	 */
	public List<AwardManpowerResource> getManpoerResourceByActualAmountNull();

	/**
	 * This method is used to get all the manpower details
	 * @return list of manpower details
	 */
	public List<Manpower> getAllManPowerDetails();

	/**
	 * This method is used to save the man power details to temp table
	 * @param manpowerTemp
	 */
	public void saveManpowerTemp(ManpowerTemp manpowerTemp);

	public List<WorkdayConfigurationData> getWorkdayConfigurationData();

	/**
	 * 
	 * @param longLeaveData
	 */
	public void saveOrUpdateLongLeaveData(WorkdayLongLeaveDetails longLeaveData);

	/**
	 * 
	 * @param terminationDetails
	 */
	public void saveOrUpdateWorkdayTerminations(WorkdayTerminationDetails terminationDetails);

	/**
	 * 
	 * @param workdayJobProfile
	 */
	public void saveOrUpdateJobProfileChanges(WorkdayJobProfileChange workdayJobProfile);

	/**
	 * 
	 * @param employeeID
	 * @return
	 */
	public List<WorkdayTerminationDetails> getTerminationDetailsByPersonId(String personId);
	
	/**
	 * 
	 * @param employeeId
	 * @return
	 */

	public List<WorkdayLongLeaveDetails> getLongLeaveDetailsByPersonId(String personId);

	/**
	 * 
	 * @param geteMPLOYEEID
	 * @return
	 */
	public WorkdayJobProfileChange getJobProfileDetailsByPersonId(String personId);

	/**
	 * 
	 * @param personIds 
	 * @param currentDate
	 * @return
	 */
	public List<AwardPersonVo> getActiveAwardsAndLeadUnitNumberByParams(List<String> personIds, Timestamp currentDate);
	
	public void deleteOldRowsBasedTerminationTriggerDate(Timestamp deleteFromDate);

	public void deleteOldRowsBasedLongLeaveTriggerDate(Timestamp deleteFromDate);

	public WorkdayLongLeaveDetails getLongLeaveDataByPersonIdAndUniqueInitiated(String personId, String initiated);

	public String getPIFullNameByAwardId(Integer awardId);
	
	public List<ManpowerLog> getManpowerLogDetails(String messageType);

	public List<AwardManpowerResource> getManpowerResourcesbyPersonIdAndWithoutName(String manpowerPersonId);
	
	public WorkdayTerminationDetails getTerminationDataByEmployeeIdAndEventInitiation(String personId, String eventInitiationDate);

	public List<AwardManpowerResource> getManpowerResourcesByParams(String positionId, List<String> statuses);

	public WorkdayManpowerInterface getLatestResourceInterfaceByParams(String resourceUniqueId, List<String> costAllocationInterfaces);

	public List<AwardManpowerResource> getAwardManpowerResourcesResourceUniqueIdAndAwardSequenceStatues(String resourceUniqueId, List<String> awardStatuses);

	public List<MigrationManpowerPerson> getallMigratedManpowerPersons();

    public Integer getCountOfPositions(String positionId);

	public AwardManpowerBaseSalaryHistory getPreviousBaseSalaryFromHistory(String personId);

	public void saveOrUpdateAwardManpowerBaseSalaryHistory(AwardManpowerBaseSalaryHistory baseSalaryHistory);

	public BigDecimal getBudgetAmountByAwardIdAndBudgetReferenceNumber(Integer awardId, String budgetReferenceNumber);

	public AwardSupOrgMapping getLatestAwardSupOrgByPI(String awardNumber, String pIPersonId);

	public List<AwardClosePositionResorceDto> getClosePositionAwardPositionsList();

	/**
	 * This method is used to select the manpower resources where resources are staff and their person id is not null
	 * @param awardId
	 * @return list manpower resources
	 */
	public List<AwardManpowerResource> getAwardManpowerResourcesByAwardId(Integer awardId);

	/**
	 * This method is used to get WorkdayManpowerInterface by Id
	 * @param workdayManpowerInterfaceId
	 * @return WorkdayManpowerInterface
	 */
	public WorkdayManpowerInterface getWorkdayManpowerInterfaceById(Integer workdayManpowerInterfaceId);

	/**
	 * This method is used to get ManpowerLog by Id
	 * @param manpowerLogId
	 * @return manpowerLogId
	 */
	public ManpowerLog getManpowerLogById(Integer manpowerLogId);

	/**
	 * This method is used to fetch the last synced time of success based on the interface type.
	 * @param manpowerInterfaceTypeCode
	 * @param manpowerInterfaceSuccess
	 * @return last synced time
	 */
	public Timestamp getLastSyncedOnTime(String manpowerInterfaceTypeCode, String manpowerInterfaceSuccess);

	/**
	 * This method is used to fetch the last synced time of success based on the interface type.
	 * @param manpowerInterfaceTypeCode
	 * @param lastSyncedSuccessTime 
	 * @param endDate 
	 * @return last synced time
	 */
	public Integer getNoOfRecords(String manpowerInterfaceTypeCode, Timestamp startDate, Timestamp endDate);

	/**
	 * This method is used to fetch the manpower log details by params
	 * @param vo
	 * @param interfaceTypeCodes 
	 * @param manpowerLogMessageType 
	 * @param interfaceStatusCodes
	 * @return vo
	 */
	public ManpowerIntegrationVO fetchManpowerLogsByParams(ManpowerIntegrationVO vo, List<String> interfaceTypeCodes, List<String> manpowerLogMessageType, List<String> interfaceStatusCodes);

	/**
	 * This method is used to fetch the workday manpower interface details by params
	 * @param vo
	 * @return vo
	 */
	public ManpowerIntegrationVO fetchManpowerTriggerDetails(ManpowerIntegrationVO vo);

	/**
	 * This method is used to fetch the workday manpower interface details by params
	 * @param resourceUniqueId
	 * @param interfaceTypeCodes 
	 * @param manpowerInterfaceStatuses 
	 * @return 
	 */
	public List<WorkdayInterfaceLogDto> fetchPositionErrorDetailsByParams(String resourceUniqueId, List<String> interfaceTypeCodes, List<String> manpowerInterfaceStatuses);

	/**
	 * This method is used to fetch the get resources 
	 * @return 
	 */
	public List<AwardManpowerResource> getLessCostAllocationResources();

	/**
	 * This method is used to fetch the list of ManpowerUserAction
	 * @return list of ManpowerUserActions
	 */
	public List<ManpowerUserAction> fetchManpowerUserActions();

	public List<AwardManpowerResource> getOwnedMigratedAwardManpowerResourcesByAwardNumber(String awardNumber);

	public Integer getPositionCountWithCreateUser(String positionId);

	/**
	 * This method is used to fetch the cost allocation interface details by params
	 * @param personId
	 * @param resourceUniqueId
	 * @return 
	 */
	public List<WorkdayInterfaceLogDto> getOtherCurrentCostAllocationDetails(String personId, String resourceUniqueId);

	/**
	 * This method is used to fetch the manpower user action name details by manpower userAction code
	 * @param manpowerUserActionCode
	 * @return manpowerUserActionName
	 */
	public String fetchManpowerUserActionNameById(String manpowerUserActionCode);

	/**
	 * This method is used get Interface Error Count By ResourceUniqueId
	 * @param resourceUniqueId
	 * @return count.
	 */
	public Integer getInterfaceErrorCountByResourceUniqueId(String resourceUniqueId);

	/**
	 * This method is used get  manpower interface status by manpowerInterfaceStatusCode
	 * @return Manpower Interface Status.
	 */
	public ManpowerInterfaceStatus fetchManpowerInterfaceStatusById(String manpowerInterfaceStatusCode);

	/**
	 * This method is used to fetch the list of WorkdayInterfaceLogs
	 * @return list of WorkdayInterfaceLogs
	 */
	public List<WorkdayInterfaceLogDto> getWorkdayInterfaceLogDtosByWorkdayManpowerInterfaceId(Integer workdayManpowerInterfaceId);

	/**
	 * This method is used to fetch ManpowerInterfaceByParams
	 * @return ManpowerInterface
	 */
	public WorkdayManpowerInterface fetchManpowerInterfaceByAwardNumberAndInterfaceTypeAndInterfaceStatus(String awardNumber, String manpowerInterfaceCreateSupOrg, String manpowerInterfacePending);

	/**
	 * This method is used to fetch Latest ManpowerInterface By AwardNumber and InterfaceType
	 * @return ManpowerInterface
	 */
	public WorkdayManpowerInterface getLatestResourceInterfaceByAwardNumberAndInterfaceType(String awardNumber,  String interfaceType);
	
	public AwardPerson getAwardPIPersonByAwardId(Integer awardId);

	/**
	 * This method is used to get resource by position id
	 * @return AwardManpowerResource
	 */
	public AwardManpowerResource getClosePositionResourceByPositionId(String positionId);

	/**
	 * This method is used to get campus by unit
	 * @return String
	 */
	public String getCampusByUnit(String unitNumber);

	/**
	 * This method is used to get RiseErrorAllocation By ResourceUniqueId
	 * @param riseErrorAllocation
	 * @return RiseErrorAllocations
	 */
	public RiseErrorAllocations getRiseErrorAllocationByResourceUniqueId(String resourceUniqueId);

	/**
	 * This method is used to save RiseErrorAllocations
	 * @param riseErrorAllocation
	 */
	public void saveRiseErrorAllocation(RiseErrorAllocations alloc);

	/**
	 * This method is used to delete RiseErrorAllocations
	 * @param riseErrorAllocation
	 */
	public void deleteRiseErrorAllocation(RiseErrorAllocations riseErrorAllocation);

}
