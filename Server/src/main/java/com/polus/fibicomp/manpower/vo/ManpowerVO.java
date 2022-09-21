package com.polus.fibicomp.manpower.vo;

import java.math.BigDecimal;
import java.util.List;

import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.manpower.dto.AwardManpowerDto;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerPayroll;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpower.pojo.ManpowerBudgetReferenceType;
import com.polus.fibicomp.manpower.pojo.ManpowerCandidateTitleType;
import com.polus.fibicomp.manpower.pojo.ManpowerCompensationType;
import com.polus.fibicomp.manpower.pojo.ManpowerInterfaceStatus;
import com.polus.fibicomp.manpower.pojo.ManpowerInterfaceType;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;
import com.polus.fibicomp.manpower.pojo.ManpowerPositionStatus;
import com.polus.fibicomp.manpower.pojo.ManpowerResourceType;
import com.polus.fibicomp.manpower.pojo.ManpowerType;
import com.polus.fibicomp.manpower.pojo.ManpowerUpgradeType;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;

public class ManpowerVO {
	
	private Integer awardId;

	private List<AwardManpower> awardManpower;

	private List<AwardManpowerResource> awardManpowerResources;

	private AwardManpowerResource awardManpowerResource;

	private List<Manpower> manpower;

	private List<ManpowerType> manpowerTypes;

	private List<ManpowerBudgetReferenceType> manpowerBudgetReferenceTypes;

	private List<ManpowerCandidateTitleType> manpowerCandidateTitleType;
	
    private List<ManpowerCompensationType> manpowerCompensationType;

    private List<ManpowerInterfaceStatus> manpowerInterfaceStatus;

    private List<ManpowerInterfaceType> manpowerInterfaceType;

    private List<ManpowerJobProfileType> manpowerJobProfileType;
  
    private List<ManpowerPositionStatus> manpowerPositionStatus;

    private List<ManpowerResourceType> manpowerResourceType;

    private String updateUser;
    
    private String budgetReferenceTypeCode;
 
    private String manpowerTypeCode;

    private String awardNumber;

    private String personId;

    private Integer sequenceNumber;

    private AwardManpowerDto manpowerCategory;

    private String createUser;

    private Integer awardBudgetHeaderId;
 
    private Integer approvedHeadCount;

    private AwardManpower awardManpowerDetail;

    private Boolean isCreateManpower = false;

    private Integer awardManpowerId;

    private Boolean isReadyToHire = false;

    private Integer manpowerResourceId;

    private String addManpowerCategoryType;

    private Boolean isManpowerCreated = false;

    private String message;

    private WorkdayManpowerInterface workdayManpowerInterface;

    private List<AwardManpowerPayroll> awardManpowerPayrolls;

    private String internalOrderCode;

    private String employeeNumber;

    private AwardBudgetHeader awardBudgetHeader;

    private Boolean isSalaryValidationExist = false;

    private BigDecimal budgetAmount = BigDecimal.ZERO;

    private Integer budgetVersionNumber;

    private Boolean isUpdateInitialCommittedAmount = false;

    private String manpowerCutOffDate;

    private Boolean isCostAllocationValidationExist = false;

    private BigDecimal validatedPlannedAmount;

    private Boolean isUpdateActualCommittedAmount = false;

    private Boolean isCommittedCostValidationExist = false;

    private BigDecimal validatedCommittedAmount;

    private String manpowerInfoText;

    private String accountNumber;

    private Boolean isBaseSalaryFieldValuesChanged = false;

	private List<String> positionIds;

    private boolean isUpgradeOrPromotion = false;

    private BigDecimal actualCommittedAmount = BigDecimal.ZERO;

    private Boolean isHiringOnExistingPosition = false;

    private BigDecimal sapCommittedAmount = BigDecimal.ZERO;

    private List<ManpowerUpgradeType> manpowerUpgradeTypes;

	public List<AwardManpower> getAwardManpower() {
		return awardManpower;
	}

	public void setAwardManpower(List<AwardManpower> awardManpower) {
		this.awardManpower = awardManpower;
	}

	public List<AwardManpowerResource> getAwardManpowerResources() {
		return awardManpowerResources;
	}

	public void setAwardManpowerResources(List<AwardManpowerResource> awardManpowerResources) {
		this.awardManpowerResources = awardManpowerResources;
	}

	public List<Manpower> getManpower() {
		return manpower;
	}

	public void setManpower(List<Manpower> manpower) {
		this.manpower = manpower;
	}

	public List<ManpowerType> getManpowerTypes() {
		return manpowerTypes;
	}

	public void setManpowerTypes(List<ManpowerType> manpowerTypes) {
		this.manpowerTypes = manpowerTypes;
	}

	public List<ManpowerBudgetReferenceType> getManpowerBudgetReferenceTypes() {
		return manpowerBudgetReferenceTypes;
	}

	public void setManpowerBudgetReferenceTypes(List<ManpowerBudgetReferenceType> manpowerBudgetReferenceTypes) {
		this.manpowerBudgetReferenceTypes = manpowerBudgetReferenceTypes;
	}

	public List<ManpowerCandidateTitleType> getManpowerCandidateTitleType() {
		return manpowerCandidateTitleType;
	}

	public void setManpowerCandidateTitleType(List<ManpowerCandidateTitleType> manpowerCandidateTitleType) {
		this.manpowerCandidateTitleType = manpowerCandidateTitleType;
	}

	public List<ManpowerCompensationType> getManpowerCompensationType() {
		return manpowerCompensationType;
	}

	public void setManpowerCompensationType(List<ManpowerCompensationType> manpowerCompensationType) {
		this.manpowerCompensationType = manpowerCompensationType;
	}

	public List<ManpowerInterfaceStatus> getManpowerInterfaceStatus() {
		return manpowerInterfaceStatus;
	}

	public void setManpowerInterfaceStatus(List<ManpowerInterfaceStatus> manpowerInterfaceStatus) {
		this.manpowerInterfaceStatus = manpowerInterfaceStatus;
	}

	public List<ManpowerInterfaceType> getManpowerInterfaceType() {
		return manpowerInterfaceType;
	}

	public void setManpowerInterfaceType(List<ManpowerInterfaceType> manpowerInterfaceType) {
		this.manpowerInterfaceType = manpowerInterfaceType;
	}

	public List<ManpowerJobProfileType> getManpowerJobProfileType() {
		return manpowerJobProfileType;
	}

	public void setManpowerJobProfileType(List<ManpowerJobProfileType> manpowerJobProfileType) {
		this.manpowerJobProfileType = manpowerJobProfileType;
	}

	public List<ManpowerPositionStatus> getManpowerPositionStatus() {
		return manpowerPositionStatus;
	}

	public void setManpowerPositionStatus(List<ManpowerPositionStatus> manpowerPositionStatus) {
		this.manpowerPositionStatus = manpowerPositionStatus;
	}

	public List<ManpowerResourceType> getManpowerResourceType() {
		return manpowerResourceType;
	}

	public void setManpowerResourceType(List<ManpowerResourceType> manpowerResourceType) {
		this.manpowerResourceType = manpowerResourceType;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getManpowerTypeCode() {
		return manpowerTypeCode;
	}

	public void setManpowerTypeCode(String manpowerTypeCode) {
		this.manpowerTypeCode = manpowerTypeCode;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public AwardManpowerDto getManpowerCategory() {
		return manpowerCategory;
	}

	public void setManpowerCategory(AwardManpowerDto manpowerCategory) {
		this.manpowerCategory = manpowerCategory;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public String getBudgetReferenceTypeCode() {
		return budgetReferenceTypeCode;
	}

	public void setBudgetReferenceTypeCode(String budgetReferenceTypeCode) {
		this.budgetReferenceTypeCode = budgetReferenceTypeCode;
	}

	public Integer getAwardBudgetHeaderId() {
		return awardBudgetHeaderId;
	}

	public void setAwardBudgetHeaderId(Integer awardBudgetHeaderId) {
		this.awardBudgetHeaderId = awardBudgetHeaderId;
	}

	public AwardManpowerResource getAwardManpowerResource() {
		return awardManpowerResource;
	}

	public void setAwardManpowerResource(AwardManpowerResource awardManpowerResource) {
		this.awardManpowerResource = awardManpowerResource;
	}

	public Integer getApprovedHeadCount() {
		return approvedHeadCount;
	}

	public void setApprovedHeadCount(Integer approvedHeadCount) {
		this.approvedHeadCount = approvedHeadCount;
	}

	public AwardManpower getAwardManpowerDetail() {
		return awardManpowerDetail;
	}

	public void setAwardManpowerDetail(AwardManpower awardManpowerDetail) {
		this.awardManpowerDetail = awardManpowerDetail;
	}

	public Integer getAwardManpowerId() {
		return awardManpowerId;
	}

	public void setAwardManpowerId(Integer awardManpowerId) {
		this.awardManpowerId = awardManpowerId;
	}

	public Boolean getIsReadyToHire() {
		return isReadyToHire;
	}

	public void setIsReadyToHire(Boolean isReadyToHire) {
		this.isReadyToHire = isReadyToHire;
	}

	public Integer getManpowerResourceId() {
		return manpowerResourceId;
	}

	public void setManpowerResourceId(Integer manpowerResourceId) {
		this.manpowerResourceId = manpowerResourceId;
	}

	public String getAddManpowerCategoryType() {
		return addManpowerCategoryType;
	}

	public void setAddManpowerCategoryType(String addManpowerCategoryType) {
		this.addManpowerCategoryType = addManpowerCategoryType;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public WorkdayManpowerInterface getWorkdayManpowerInterface() {
		return workdayManpowerInterface;
	}

	public void setWorkdayManpowerInterface(WorkdayManpowerInterface workdayManpowerInterface) {
		this.workdayManpowerInterface = workdayManpowerInterface;
	}

	public String getEmployeeNumber() {
		return employeeNumber;
	}

	public void setEmployeeNumber(String employeeNumber) {
		this.employeeNumber = employeeNumber;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public AwardBudgetHeader getAwardBudgetHeader() {
		return awardBudgetHeader;
	}

	public void setAwardBudgetHeader(AwardBudgetHeader awardBudgetHeader) {
		this.awardBudgetHeader = awardBudgetHeader;
	}

	public BigDecimal getBudgetAmount() {
		return budgetAmount;
	}

	public void setBudgetAmount(BigDecimal budgetAmount) {
		this.budgetAmount = budgetAmount;
	}

	public List<AwardManpowerPayroll> getAwardManpowerPayrolls() {
		return awardManpowerPayrolls;
	}

	public void setAwardManpowerPayrolls(List<AwardManpowerPayroll> awardManpowerPayrolls) {
		this.awardManpowerPayrolls = awardManpowerPayrolls;
	}

	public Integer getBudgetVersionNumber() {
		return budgetVersionNumber;
	}

	public void setBudgetVersionNumber(Integer budgetVersionNumber) {
		this.budgetVersionNumber = budgetVersionNumber;
	}

	public Boolean getIsUpdateInitialCommittedAmount() {
		return isUpdateInitialCommittedAmount;
	}

	public void setIsUpdateInitialCommittedAmount(Boolean isUpdateInitialCommittedAmount) {
		this.isUpdateInitialCommittedAmount = isUpdateInitialCommittedAmount;
	}

	public String getManpowerCutOffDate() {
		return manpowerCutOffDate;
	}

	public void setManpowerCutOffDate(String manpowerCutOffDate) {
		this.manpowerCutOffDate = manpowerCutOffDate;
	}

	public Boolean getIsCostAllocationValidationExist() {
		return isCostAllocationValidationExist;
	}

	public void setIsCostAllocationValidationExist(Boolean isCostAllocationValidationExist) {
		this.isCostAllocationValidationExist = isCostAllocationValidationExist;
	}

	public BigDecimal getValidatedPlannedAmount() {
		return validatedPlannedAmount;
	}

	public void setValidatedPlannedAmount(BigDecimal validatedPlannedAmount) {
		this.validatedPlannedAmount = validatedPlannedAmount;
	}

	public Boolean getIsSalaryValidationExist() {
		return isSalaryValidationExist;
	}

	public void setIsSalaryValidationExist(Boolean isSalaryValidationExist) {
		this.isSalaryValidationExist = isSalaryValidationExist;
	}

	public BigDecimal getValidatedCommittedAmount() {
		return validatedCommittedAmount;
	}

	public void setValidatedCommittedAmount(BigDecimal validatedCommittedAmount) {
		this.validatedCommittedAmount = validatedCommittedAmount;
	}

	public String getManpowerInfoText() {
		return manpowerInfoText;
	}

	public void setManpowerInfoText(String manpowerInfoText) {
		this.manpowerInfoText = manpowerInfoText;
	}

	public List<String> getPositionIds() {
		return positionIds;
	}

	public void setPositionIds(List<String> positionIds) {
		this.positionIds = positionIds;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public Boolean getIsBaseSalaryFieldValuesChanged() {
		return isBaseSalaryFieldValuesChanged;
	}

	public void setIsBaseSalaryFieldValuesChanged(Boolean isBaseSalaryFieldValuesChanged) {
		this.isBaseSalaryFieldValuesChanged = isBaseSalaryFieldValuesChanged;
	}

	public Boolean getIsCreateManpower() {
		return isCreateManpower;
	}

	public void setIsCreateManpower(Boolean isCreateManpower) {
		this.isCreateManpower = isCreateManpower;
	}

	public Boolean getIsManpowerCreated() {
		return isManpowerCreated;
	}

	public void setIsManpowerCreated(Boolean isManpowerCreated) {
		this.isManpowerCreated = isManpowerCreated;
	}

	public Boolean getIsUpdateActualCommittedAmount() {
		return isUpdateActualCommittedAmount;
	}

	public void setIsUpdateActualCommittedAmount(Boolean isUpdateActualCommittedAmount) {
		this.isUpdateActualCommittedAmount = isUpdateActualCommittedAmount;
	}

	public Boolean getIsCommittedCostValidationExist() {
		return isCommittedCostValidationExist;
	}

	public void setIsCommittedCostValidationExist(Boolean isCommittedCostValidationExist) {
		this.isCommittedCostValidationExist = isCommittedCostValidationExist;
	}

	public boolean isUpgradeOrPromotion() {
		return isUpgradeOrPromotion;
	}

	public void setUpgradeOrPromotion(boolean isUpgradeOrPromotion) {
		this.isUpgradeOrPromotion = isUpgradeOrPromotion;
	}

	public BigDecimal getActualCommittedAmount() {
		return actualCommittedAmount;
	}

	public void setActualCommittedAmount(BigDecimal actualCommittedAmount) {
		this.actualCommittedAmount = actualCommittedAmount;
	}

	public Boolean getIsHiringOnExistingPosition() {
		return isHiringOnExistingPosition;
	}

	public void setIsHiringOnExistingPosition(Boolean isHiringOnExistingPosition) {
		this.isHiringOnExistingPosition = isHiringOnExistingPosition;
	}

	public BigDecimal getSapCommittedAmount() {
		return sapCommittedAmount;
	}

	public void setSapCommittedAmount(BigDecimal sapCommittedAmount) {
		this.sapCommittedAmount = sapCommittedAmount;
	}

	public List<ManpowerUpgradeType> getManpowerUpgradeTypes() {
		return manpowerUpgradeTypes;
	}

	public void setManpowerUpgradeTypes(List<ManpowerUpgradeType> manpowerUpgradeTypes) {
		this.manpowerUpgradeTypes = manpowerUpgradeTypes;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

}
