package com.polus.fibicomp.manpower.service;

import java.math.BigDecimal;
import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.manpower.dto.AwardManpowerDto;
import com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;
import com.polus.fibicomp.manpower.vo.ManpowerVO;


@Transactional
@Service
public interface ManpowerService {

	/**
	 * This method is used to fetch all manpower look up data.
	 * @return A list of look up data.
	 */
	public String fetchAllManpowerLookUpDatas(Integer awardId);

	/**
	 * This method is used to create manpower plan.
	 * @return A list of award manpower.
	 */
	public String createManpowerPlan(ManpowerVO vo);

	/**
	 * This method is used to prepare manpower data.
	 * @return A list of award manpower data.
	 */
	public AwardManpowerDto prepareAwardManpowerDetails(ManpowerVO vo);

	/**
	 * This method is used fetch manpower details
	 * @param vo
	 * @return A list of award manpower deatils.
	 */
	public String fetchManpowerDetails(ManpowerVO vo);

	/**
	 * This method is used to save or update award manpower resource
	 * @param vo
	 * @return vo
	 */
	public ManpowerVO saveOrUpdateManpowerResource(ManpowerVO vo);

	/**
	 * This method is used to save or update award manpower
	 * @param vo
	 * @return vo
	 */
	public String saveOrUpdateAwardManpower(ManpowerVO vo);

	/**
	 * This method is used to delete award manpower resource
	 * @param vo
	 * @return vo
	 */
	public String deleteManpowerResource(ManpowerVO vo);

	/**
	 * This method is used to save or update award manpower details
	 * @param vo
	 * @param request
	 * @return vo
	 */
	public String saveOrUpdateManpowerTriggerDetail(ManpowerVO vo);

	/**
	 * This method is used to update award manpower details
	 * @param vo
	 * @return vo
	 */
	public String updateManpowerDetails(ManpowerVO vo);

	/**
	 * This method is used to prepare award manpower details based on budget category
	 * @param vo
	 * @param awardBudgetDetail
	 * @param actualHeadCount
	 * @param approvedHeadCount
	 * @param awardBudgetHeader
	 * @return vo
	 */
	public ManpowerVO setManpowerBudgetCategoryDetails(ManpowerVO vo, AwardBudgetDetail awardBudgetDetail, Integer actualHeadCount, Integer approvedHeadCount, Integer budgetVersionNumber);

	/**
	 * This method is used to prepare award manpower details
	 * @param vo
	 * @param awardBudgetDetail
	 * @param actualHeadCount
	 * @param approvedHeadCount
	 * @param awardBudgetHeader
	 * @return vo
	 */
	public ManpowerVO setAwardManpowerDetails(AwardBudgetDetail awardBudgetDetail, Integer actualHeadCount,	Integer approvedHeadCount, Integer budgetVersionNumber, ManpowerVO vo);

	/**
	 * This method is used to prepare award manpower details
	 * @param newAward
	 * @param activeAward
	 * @param activeAwardSuperiorSupOrg 
	 * @param activeAwardPIPerson 
	 * @param newAwardPIPerson
	 * @return 
	 * @throws Exception 
	 */
	public void saveWorkdayManpowerInterfaceDetails(Award newAward, Award activeAward, AwardPerson newAwardPIPerson, AwardPerson activeAwardPIPerson, String activeAwardSuperiorSupOrg) throws Exception;

	/**
	 * This method is used to get award manpower payroll details
	 * @param vo
	 * @return 
	 */
	public String fetchManpowerPayrollDetails(ManpowerVO vo);

	/**
	 * This method is used to get persons with position id
	 * @param searchString
	 * @param isGraduateStudent
	 * @param awardId
	 * @param manpowerRequestType 
	 * @return object of persons
	 */
	public List<ManpowerPersonSearchResult>  getPersonsWithPositionId(String searchString, Boolean isGraduateStudent , String awardId, String manpowerRequestType);

	/**
	 * This method is used to get job profile
	 * @param searchString
	 * @param costElementCode
	 * @return object job profile
	 */
	public List<ManpowerJobProfileType> getManpowerJobProfile(String searchString, String costElementCode);

	/**
	 * This method is used to get configuration value.
	 * @param cofigurationValue
	 * @return string value
	 */
	public String getManpowerConfigurationValueAsString(String cofigurationValue);

	/**
	 * This method is used to get sum of committed cost based on position status
	 * @param awardManpowerId
	 * @param manpowerTypeCode 
	 * @return sum
	 */
	public BigDecimal calculateBudgetActualCommittedCost(Integer awardManpowerId, String manpowerTypeCode);

	public BigDecimal calculatePlannedCost(AwardManpowerResource awardManpowerResource, String awardId,
			Boolean isActualCommittedAmount, String multiplier);
	
	/**
	 * This method is used to calculate initial base salary
	 * @param vo
	 * @param request
	 * @return vo
	 */
	public String calculatePlannedSalary(ManpowerVO vo);

	/**
	 * This method is used for manpower closing position scheduler 
	 */
	public void manpowerClosingPosition();

	/**
	 * This method is used to find multiplier value
	 * @param awardId
	 * @return multiplier
	 */
	public String findMultiplier(Integer awardId);

	/**
	 * This method is used to override actual committed amount
	 */
	public String overrideActualCommittedAmount(ManpowerVO vo);

	/**
	 * This method is used to check salary validation
	 */
	public ManpowerVO setInitialCommittedAmount(ManpowerVO vo, AwardManpower awardManpower, BigDecimal resourceCommittedCost, BigDecimal manpowerResourceAmount, Integer awardManpowerResourceId);

	/**
	 * This method is used to find Superior for award
	 * @throws Exception 
	 */
	public String findSuperiorSupOrgForAward(Award award) throws Exception;

	/**
	 * This method is used to fetch the Manpower base salary details
	 */
	public ResponseEntity<String> fetchManpowerBaseSalaryDetails(String awardNumber, String personId, String accountNumber);

	/**
	 * This method is used to fetch the Manpower details for award comparison
	 * @return manpower details
	 */
	public String fetchAwardManpowerForComparison(Integer awardId);

}
