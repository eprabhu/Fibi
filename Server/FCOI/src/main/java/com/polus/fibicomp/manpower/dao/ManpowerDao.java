package com.polus.fibicomp.manpower.dao;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerPayroll;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpower.pojo.ManpowerBudgetReferenceType;
import com.polus.fibicomp.manpower.pojo.ManpowerCandidateTitleType;
import com.polus.fibicomp.manpower.pojo.ManpowerCompensationType;
import com.polus.fibicomp.manpower.pojo.ManpowerConfigurationData;
import com.polus.fibicomp.manpower.pojo.ManpowerInterfaceStatus;
import com.polus.fibicomp.manpower.pojo.ManpowerInterfaceType;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;
import com.polus.fibicomp.manpower.pojo.ManpowerLogUser;
import com.polus.fibicomp.manpower.pojo.ManpowerPositionStatus;
import com.polus.fibicomp.manpower.pojo.ManpowerResourceType;
import com.polus.fibicomp.manpower.pojo.ManpowerType;
import com.polus.fibicomp.manpower.pojo.ManpowerUpgradeType;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;

@Transactional
@Service
public interface ManpowerDao {

	/**
	 * This method is used to fetch all getManpowerTypes.
	 * @return A list of ManpowerType.
	 */
	public List<ManpowerType> getManpowerTypes();

	/**
	 * This method is used to fetch all ManpowerBudgetReferenceType.
	 * @return A list of ManpowerBudgetReferenceType.
	 */
	public List<ManpowerBudgetReferenceType> getManpowerBudgetReferenceType();

	/**
	 * This method is used to fetch all ManpowerCandidateTitleType.
	 * @return A list of ManpowerCandidateTitleType.
	 */
	public List<ManpowerCandidateTitleType> getManpowerCandidateTitleType();

	/**
	 * This method is used to fetch all ManpowerCompensationType.
	 * @return A list of ManpowerCompensationType.
	 */
	public List<ManpowerCompensationType> getManpowerCompensationType();

	/**
	 * This method is used to fetch all ManpowerInterfaceStatus.
	 * @return A list of ManpowerInterfaceStatus.
	 */
	public List<ManpowerInterfaceStatus> getManpowerInterfaceStatus();

	/**
	 * This method is used to fetch all ManpowerInterfaceType.
	 * @return A list of ManpowerInterfaceType.
	 */
	public List<ManpowerInterfaceType> getManpowerInterfaceType();

	/**
	 * This method is used to fetch all ManpowerJobProfileType.
	 * @return A list of ManpowerJobProfileType.
	 */
	public List<ManpowerJobProfileType> getManpowerJobProfileType();

	/**
	 * This method is used to fetch all ManpowerPositionStatus.
	 * @return A list of ManpowerPositionStatus.
	 */
	public List<ManpowerPositionStatus> getManpowerPositionStatus();

	/**
	 * This method is used to fetch all ManpowerResourceType.
	 * @return A list of ManpowerResourceType.
	 */
	public List<ManpowerResourceType> getManpowerResourceType();

	/**
	 * This method is used to check award manpower exist or not
	 * @param awardId
	 * @return true or false
	 */
	public Boolean checkIfAwardManpowerIsExistBasedOnParams(Integer awardId);

	/**
	 * This method is used to save or update award manpower resource
	 * @param awardManpowerResource
	 * @return awardManpowerResource
	 */
	public AwardManpowerResource saveOrUpdateAwardManpowerResources(AwardManpowerResource awardManpowerResource);


	/**
	 * This method is used to save or update award manpower 
	 * @param awardManpower
	 * @return awardManpower
	 */
	public AwardManpower saveOrUpdateAwardManpower(AwardManpower awardManpower);

	/**
	 * This method is used to get award manpower details
	 * @param awardId
	 * @param manpowerTypeCode
	 * @return list of AwardManpower
	 */
	public List<AwardManpower> getAwardManpowerDetails(Integer awardId, String manpowerTypeCode);

	/**
	 * This method is used to get AwardBudgetDetail based on params
	 * @param awardBudgetDetailId
	 * @param wbsNumber
	 * @param awardBudgetHeaderId
	 * @return object of AwardBudgetDetail
	 */
	public AwardBudgetDetail getAwardBudgetDetailByParams(Integer awardBudgetDetailId, String wbsNumber, Integer awardBudgetHeaderId);

	/**
	 * This method is used to get ManpowerType based on manpowerTypeCode
	 * @param manpowerTypeCode
	 * @return object of ManpowerType
	 */
	public ManpowerType getManpowerTypeBasedOnCode(String manpowerTypeCode);

	/**
	 * This method is used to get AwardManpower based on awardManpowerId
	 * @param awardManpowerId
	 * @return object of AwardManpower
	 */
	public AwardManpower fetchAwardManpowerDetailByAwardManpowerId(Integer awardManpowerId);

	/**
	 * This method is used to get count of persons based on params
	 * @param awardManpowerId
	 * @param postionStatusCode
	 * @return count
	 */
	public Integer countOfActiveManpowerResource(Integer awardManpowerId);

	/**
	 * This method is used to get all AwardManpowerResource based on awardManpowerId
	 * @param awardManpowerId
	 * @return list of AwardManpowerResource
	 */
	public List<AwardManpowerResource> getAwardManpowerResources(Integer awardManpowerId);

	/**
	 * This method is used to get all budget reference numbers based on awardId
	 * @param awardId
	 * @return list of budget reference numbers
	 */
	public List<AwardManpower> getBudgetReferenceNumbers(Integer awardId);

	/**
	 * This method is used to get delete award manpower resource based on manpowerResourceId
	 * @param manpowerResourceId
	 * @return message
	 */
	public String deleteManpowerResource(Integer manpowerResourceId);

	/**
	 * This method is used to get Manpower based on params
	 * @param positionId
	 * @param personId
	 * @return object of Manpower
	 */
	public Manpower fetchManpowerPersonDetail(String positionId, String personId);

	/**
	 * This method is used to get ManpowerPositionStatus based on param
	 * @param positionStatusCode
	 * @return object of ManpowerPositionStatus
	 */
	public ManpowerPositionStatus getManpowerPositionStatusById(String positionStatusCode);

	/**
	 * This method is used to get ManpowerCompensationType based on param
	 * @param planCompensationTypeCode
	 * @return object of ManpowerCompensationType
	 */
	public ManpowerCompensationType getManpowerPlanCompensationTypeById(String planCompensationTypeCode);

	/**
	 * This method is used to get ManpowerJobProfileType based on param
	 * @param planJobProfileTypeCode
	 * @return object of ManpowerJobProfileType
	 */
	
	public ManpowerJobProfileType getManpowerJobProfileTypeById(String jobProfileTypeCode);

	/**
	 * This method is used tosave or update WorkdayManpowerInterface
	 * @param workdayManpowerInterface
	 * @return object of WorkdayManpowerInterface
	 */
	public WorkdayManpowerInterface saveOrUpdateWorkdayManpowerInterface(WorkdayManpowerInterface workdayManpowerInterface);

	/**
	 * This method is used to get AwardManpowerResource based on param
	 * @param awardManpowerResourceId
	 * @return object of AwardManpowerResource
	 */
	public AwardManpowerResource getAwardManpowerResourceById(Integer awardManpowerResourceId);

	/**
	 * This method is used to get sum of line item cost based on params
	 * @param awardId
	 * @param budgetCategoryCode
	 * @return sum
	 */
	public BigDecimal fetchAwardBudgetDetailsLineItemCostByCategory(Integer awardBudgetDetailId, String internalOrderCode, Integer awardBudgetHeaderId);

	/**
	 * This method is used to get ManpowerResourceType based on param
	 * @param resourceTypeCode
	 * @return object of ManpowerResourceType
	 */
	public ManpowerResourceType getManpowerResourceTypeById(String resourceTypeCode);

	/**
	 * This method is used to get ManpowerCandidateTitleType based on param
	 * @param candidateTitleTypeCode
	 * @return object of ManpowerCandidateTitleType
	 */
	public ManpowerCandidateTitleType getManpowerCandidateTitleTypeById(String candidateTitleTypeCode);

	/**
	 * This method is used to get AwardManpowerPayroll based on param
	 * @param internalOrderCode
	 * @param employeeNumber
	 * @return list of AwardManpowerPayroll
	 */
	public List<AwardManpowerPayroll>  fetchManpowerPayrollDetails(String internalOrderCode, String employeeNumber);

	/**
	 * This method is used to get Sum of Resources Committed Amount based on param
	 * @param awardManpowerId
	 * @param manpowerResourceId
	 * @return sum of committed amount
	 */
	public BigDecimal getSumOfResourcesCommittedAmount(Integer awardManpowerId, Integer manpowerResourceId);

	/**
	 * This method is used to fetch award manpower details based on params
	 * @param budgetReferenceNumber
	 * @param awardId
	 * @return object of AwardManpower
	 */
	public AwardManpower fetchAwardManpowerDetails(String budgetReferenceNumber, Integer awardId);

	/**
	 * This method is used to find the person with position id
	 * @param searchString
	 * @return person details
	 */
	public List<ManpowerPersonSearchResult> findPersonWithPositionId(String searchString);

	/**
	 * This method is used to check the award manpower resources exist
	 * @param awardManpowerId
	 * @return boolean value
	 */

	public Boolean checkIfAwardManpowerResourceIsExistBasedOnParams(Integer awardManpowerId);

	/**
	 * This method is used to delete award manpower
	 * @param awardManpowerId
	 * @return message
	 */
	public String deleteAwardManpower(Integer awardManpowerId);

	/**
	 * This method is used to get award expense transaction details based on params
	 * @param fiGlAccount
	 * @param internalOrderCode
	 * @return list of award expense transaction
	 */
	public List<AwardExpenseTransaction> getAwardExpenseTransactionDetails(String fiGlAccount, String internalOrderCode);

	/**
	 * This method is used to get Sum of payroll Amount based on params
	 * @param awardManpowerId
	 * @param awardManpowerResourceId
	 * @return sum of payroll amount
	 */
	public BigDecimal getsumOfPayrollAmount(String internalOrderCode, String employeeNumber);

	/**
	 * This method is used to get distinct position ids
	 * @param awardManpowerId
	 * @return position ids
	 */
	public List<String> getDistinctPositionIds(Integer awardManpowerId);

	/**
	 * This method is used to get Sum of Planned Amount based on params
	 * @param awardManpowerId
	 * @param awardManpowerResourceId
	 * @return sum of planned amount
	 */
	public BigDecimal getSumOfResourcesPlannedSalary(Integer awardManpowerId, Integer awardManpowerResourceId);

	/**
	 * This method is used to get job profile
	 * @param searchString
	 * @param costElementCode
	 * @return list job profile
	 */
	public List<ManpowerJobProfileType> getManpowerJobProfile(String searchString, String costElementCode);

	/**
	 * This method is used to delete all award manpower resources
	 * @param awardManpowerResource
	 * @return awardManpowerResource
	 */
	public List<AwardManpowerResource> deleteAllManpowerResource(List<AwardManpowerResource> awardManpowerResource);

	/**
	 * This method is used to delete all awardManpower
	 * @param awardManpower
	 * @return awardManpower
	 */
	public List<AwardManpower> deleteAllAwardManpower(List<AwardManpower> awardManpower);

	/**
	 * This method is used to delete all resources from workday
	 * @param workdayManpowerInterface
	 * @return workdayManpowerInterface
	 */
    public List<WorkdayManpowerInterface> deleteAllResourcesInWorkday(List<WorkdayManpowerInterface> workdayManpowerInterface);

    /**
	 * This method is used to get all workday details by params
	 * @param workdayManpowerInterface
	 * @return list of workdayManpowerInterface
	 */
	public List<WorkdayManpowerInterface> getAwardManpowerWorkday(String awardId);

	/**
	 * This method is used to get all award manpower resources
	 * @param awardManpowerIds
	 * @return list of awardManpower resources
	 */
	public List<AwardManpowerResource> getAllManpowerResources(Set<Integer> awardManpowerIds);

	/**
	 * This method is used to get manpower details based on manpower typeCodes
	 * @param awardId
	 * @param manpowerTypeCode
	 * @return list of awardManpower 
	 */
	public List<AwardManpower> getAwardManpowerDetailsBasedTypeCodes(Integer awardId, String manpowerTypeCode);

	/**
	 * This method is used to check award manpower exist in workday
	 * @param awardManpowerResourceId
	 * @return boolean value 
	 */
	public Boolean checkIfAwardManpowerResourceIsExistInWorkday(Integer awardManpowerResourceId);

	/**
	 * This method is used to check award manpower cost allocation exceeds
	 * @param personId
	 * @param costAllocation
	 * @param startDate
	 * @param endDate
	 * @return String value 
	 */
	public String getAwardManpowerCostAllocation(String personId, BigDecimal costAllocation, Timestamp startDate, Timestamp endDate, String awardNumber);

	/**
	 * This method is used to get sum of plannesSalary
	 * @param awardManpowerId
	 * @return sum of planned salary
	 */
	public BigDecimal getSumOfPlannedSalary(Integer awardManpowerId);

	/**
	 * This method is used to find person
	 * @param searchString
	 * @return list of person
	 */
	public List<ManpowerPersonSearchResult> findGraduateStudents(String searchString);

	/**
	 * This method is used to get configuration value.
	 * @param cofigurationValue
	 * @return string value
	 */
	public ManpowerConfigurationData getManpowerConfigurationValue(String configurationKey);

	/**
	 * This method is used to get sum of committed amount based on position status  
	 * @param awardManpowerId
	 * @return sum
	 */
	public BigDecimal getSumOfCommittedAmount(Integer awardManpowerId);

	/**
	 * This method is used to get sum of initial committed amount based on position status
	 * @param awardManpowerId
	 * @return sum
	 */
	public BigDecimal getSumOfInitialCommittedAmount(Integer awardManpowerId);

	/**
	 * This method is used to update manpower resource unique id
	 * @param resourceUniqueId
	 * @param manpowerResourceId
	 */
	public void updateManpowerResourceUniqueId(String resourceUniqueId, Integer manpowerResourceId);

	/**
	 * This method is used to check the award contains positions id
	 * @param awardId
	 * @param positionId
	 * @param count
	 */
    public Boolean checkPositionIdExistInAwardManpower(Integer awardId, String positionId, String personId);

    /**
	 * This method is used to get manpower resources based on cost allocation
	 * @param personId
	 * @param startDate
	 * @param endDate
	 * @param awardNumber
	 * @param resourceFlag
	 * @param list of awardManpowerResource.
	 */
	public List<AwardManpowerResource> getManpowerResourceBasedCostAllocation(String personId, Timestamp startDate, Timestamp endDate, String awardNumber, String resourceFlag);

	/**
	 * This method is used to find the manpower persons based o position owned by award
	 * @param awardId 
	 * @param searchString
	 * @param manpowerRequestType 
	 * @return list of persons
	 */
	public List<ManpowerPersonSearchResult> findManpowerPerson(Integer awardId, String searchString,  String manpowerRequestType);

	/**
	 * This method is used to find the job profile type codeby param
	 * @param personId 
	 * @param positionId
	 * @return list of awardManpowerResource
	 */
	public List<AwardManpowerResource> getResourceJobProfileByParam(String personId, String positionId);

	/**
	 * This method is used get award details by resource id
	 * @param manpowerResourceId.
	 * @return award.
	 */
	public Award getAwardByResourceId(Integer manpowerResourceId);

	/**
	 * This method is used to check the manpower section code is editable or not for the module item
	 * @param moduleItemKey
	 * @param subModuleItemKey
	 * @param moduleCode
	 * @param subModuleCode
	 * @return true or false
	 */
	boolean isManpowerSectionEditable(String moduleItemKey, String subModuleItemKey, Integer moduleCode, Integer subModuleCode);

	/**
	 * This method is used get active manpower resources
	 * @return list of AwardManpowerResource.
	 */
	public List<AwardManpowerResource> getActiveManpowerResources();

	/**
	 * This method is used get manpower resources
	 * @return list of resource Ids.
	 */
	public List<Integer> getAwardManpowerResourcesByParam(Integer awardManpowerId);

	/**
	 * This method is used get manpower multiplier used
	 * @return .
	 */
	public BigDecimal getResourceMultiplierUsed(Integer manpowerResourceId);

	/**
	 * This method is used get manpower upgrade types
	 * @return list of types.
	 */
	public List<ManpowerUpgradeType> getManpowerUpgradeType();

	/**
	 * This method is used get manpower interface type by Id
	 * @return ManpowerInterfaceType.
	 */
	public ManpowerInterfaceType fetchManpowerInterfaceById(String manpowerInterfaceTypeCode);

	/**
	 * This method is used get list of manpower interface type by Ids
	 * @return list of ManpowerInterfaceType.
	 */
	public List<ManpowerInterfaceType> fetchManpowerInterfacesByIds(List<String> manpowerInterfaceTypeCodes);

	/**
	 * This method is used to get list of manpower base salary details for the given personId and awardNumber
	 */
	public List<AwardManpowerResource> fetchManpowerBaseSalaryDetailsByAwardNumberAndPersonId(String awardNumber, String accountNumber, String personId);

	/**
	 * This method is used to save manpower log user details
	 */

	public void saveManpowerLogUserDetails(ManpowerLogUser manpowerLogUser);

	/**
	 * This method is used to get award budget details
	 * @param budgetId - budgetId
	 * @param wbsNumbers -wbsNumbers
	 * @param budgetDetailsIds - budgetDetailsIds
	 */
	public List<AwardBudgetDetail>  getAwardBudgetDetailForManpower(Integer budgetId, List<String> wbsNumbers, List<String> budgetDetailsIds);

	/**
	 * This method is used to get award budget header details
	 * @param awardId - awardId
	 */
	public AwardBudgetHeader getAwardBudgetByVersionNumber(Integer awardId);

  /**
	 * This method is used get  manpower resources by award Id
	 * @param awardId
	 * @return list of Award Manpower resources.
	 */
	public List<AwardManpowerResource> fetchAwardManpowerResourcesByAwardId(Integer awardId);

	/**
	 * This method is used get  manpower resources by award Id and budgetReferenceNumbers
	 * @param awardId
	 * @param budgetReferenceNumbers
	 * @return list of Award Manpower resources.
	 */
    public List<AwardManpower> fetchAwardManpowerDetailsByIOCode(Set<String> budgetReferenceNumbers, Integer awardId);

    /**
	 * This method is used get  manpower resources by awardManpowerIds
	 * @param awardManpowerIds
	 * @return list of Award Manpower resources.
	 */
	List<AwardManpowerResource> getAllAwardManpowerResourcesByManpowerIds(Set<Integer> awardManpowerIds);

    /**
	 * This method is used get  manpower resources by awardManpowerId
	 * @param awardManpowerId
	 * @return list of Award Manpower resources.
	 */
	public List<AwardManpowerResource> getAwardManpowerResourcesByManpowerId(Integer awardManpowerId);

}
