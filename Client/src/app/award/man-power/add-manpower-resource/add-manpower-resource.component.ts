import { Component, EventEmitter, Input, OnChanges, OnDestroy, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { ElasticConfigService } from '../../../common/services/elastic-config.service';
import { getEndPointOptionsForManpower } from '../../../common/services/end-point.config';
import { slideInOut } from '../../../common/utilities/animations';
import {
    convertToValidAmount, inputRestrictionForAmountField, setFocusToElement,
    validatePercentage
} from '../../../common/utilities/custom-utilities';
import {
    compareDates, getDateObjectFromTimeStamp, getDuration,
    parseDateWithoutTimestamp
} from '../../../common/utilities/date-utilities';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { CommonDataService } from '../../services/common-data.service';
import { ManPowerService } from '../man-power.service';
import {
    addResourceToList, calculateArraySum, calculateUncommittedAmount, checkCutoffDate,
    checkDuplicationExistingHire, compareDatesForOverlap, getNumberOfDays,
    getSystemDate,
    newExistingPositionOverlap,
    personDuplicationCheck, updateEditedManpowerResource, validateApproverHeadCount
} from '../manpower-utilities';
declare var $: any;

@Component({
    selector: 'app-add-manpower-resource',
    templateUrl: './add-manpower-resource.component.html',
    styleUrls: ['./add-manpower-resource.component.css'],
    animations: [slideInOut]
})
export class AddManpowerResourceComponent implements OnChanges, OnDestroy {
    @Input() manpowerCategory: any;
    @Input() resourceCategory: any;
    @Input() awardData: any;
    @Input() helpText: any;
    @Input() manpowerList: any;
    @Input() manpowerLookups: any;
    @Output() resourceOperations: EventEmitter<any> = new EventEmitter<any>();
    $subscriptions: Subscription[] = [];
    isShowManpowerInfo = true;
    map = new Map();
    datePlaceHolder = DEFAULT_DATE_FORMAT;
    candidateTitle: any;
    resourceDetails: any = {};
    isResourceInSamePositionId = false;
    resourceSearchOption: any = {};
    employeeSearchOption: any = {};
    jobProfileSearchOption: any = {};
    clearJobProfile: String;
    clearField: String;
    positionId: any;
    isReadyToHire: any;
    resourceType: any;
    isEmployeeFlag = true;
    canEditStartDate = false;
    isCostAllocationValidation: string;
    isSalaryValidation: string;
    setFocusToElement = setFocusToElement;
    isAddNonEmployeeModal = false;
    isSaving = false;
    manpowerWarning: any = [];
    costAllocationList: any = [];
    upgradeOrPromotionValue: any = null;
    canModifyChargeEndDate = false;
    isCostAllocationFocused = false;
    remainingCAFromWBS: any = null;
    previousUpgradeType: any;
    isChangeInCalculationRelatedFields = false;

    constructor(public _commonService: CommonService, private _commonData: CommonDataService,
        private _elasticConfig: ElasticConfigService, private _manpowerService: ManPowerService) { }

    ngOnChanges() {
        this.initializeModal();
        this.setHeadCountWarning();
        this.resourceCategory && this.resourceCategory.index === null ? this.setDataForNewResource() : this.setDataForEditResource();
        $('#addManpowerResource').modal('show');
    }
    /**
     * section code 131 is for manpower staff variation
     */
    getPermissions(): void {
        this.canEditStartDate = this._commonData.getSectionEditableFlag('131') &&
            this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_MODIFY_STAFF_CHARGE_START_DATE');
        this.canModifyChargeEndDate = this._commonData.getSectionEditableFlag('131') &&
            this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_MODIFY_STAFF_CHARGE_END_DATE');
    }

    initializeModal(): void {
        this.map.clear();
        this.manpowerWarning = [];
        this.upgradeOrPromotionValue = null;
        this.resourceDetails = {};
        this.candidateTitle = null;
        this.resourceType = null;
        this.positionId = null;
        this.isReadyToHire = false;
        this.previousUpgradeType = null;
        this.isChangeInCalculationRelatedFields = false;
        this.isSalaryValidation = '';
        this.isCostAllocationValidation = '';
        if (this.resourceCategory.categoryType === 'Staff') {
            this.setJobProfileOptions();
        }
        this.setResourceSearchOption();
        this.changeResourceType();
    }

    setJobProfileOptions(): void {
        this.jobProfileSearchOption = getEndPointOptionsForManpower('description (jobFamily)', 'description (jobFamily)', 'findManpowerJobProfile',
        { costElementCode: this.manpowerCategory.costElement.costElement });
    }

    setResourceSearchOption(): void {
        this.resourceSearchOption = getEndPointOptionsForManpower('fullName', 'fullName (userName)', 'findPersonsWithPositionId',
            {
                'isGraduateStudent': this.resourceCategory.categoryType !== 'Staff', 'awardId': this.awardData.award.awardId,
                'manpowerRequestType': this.upgradeOrPromotionValue
            });
    }

    setDataForNewResource(): void {
        if (this.resourceCategory.categoryType === 'Staff') {
            this.resourceDetails.planDuration = '0 year(s) , 0 month(s) & 0 day(s)';
            this.resourceDetails.costAllocation = this.resourceCategory.addStaffType === 'New' ? '100' : '';
        }
        if (this.resourceCategory.categoryType !== 'Staff') {
            this.resourceDetails.chargeDuration = '0 year(s) , 0 month(s) & 0 day(s)';
        }
    }

    setDataForEditResource(): void {
        this.resourceDetails = Object.assign({}, this.resourceCategory.resourceObject);
        this.resourceSearchOption.defaultValue = this.resourceCategory.categoryType !== 'Others' &&
            this.resourceCategory.addStaffType !== 'New' ? this.resourceDetails.fullName : '';
        this.candidateTitle = this.resourceCategory.categoryType === 'Student' ?
            this.resourceDetails.manpowerCandidateTitleType.candidateTitleTypeCode : null;
        if (this.resourceCategory.categoryType === 'Others') {
            this.isEmployeeFlag = !this.resourceCategory.resourceObject.rolodexId;
            (this.isEmployeeFlag) ? this.setElasticPersonOption() : this.setElasticRolodexOption();
            this.employeeSearchOption.defaultValue = this.resourceDetails.fullName;
            this.resourceType = this.resourceDetails.manpowerResourceType.resourceTypeCode;
        }
        if (this.resourceCategory.categoryType === 'Staff' && this.resourceCategory.addStaffType === 'New') {
            this.isResourceInSamePositionId = this.resourceDetails.manpowerPositionStatus.positionStatusCode === '7';
            this.positionId = this.isResourceInSamePositionId ? this.resourceDetails.positionId : null;
        }
        if (this.resourceCategory.categoryType === 'Staff') {
            this.upgradeOrPromotionValue = this.resourceDetails.upgradeTypeCode;
            this.isReadyToHire = this.resourceDetails.manpowerPositionStatus.positionStatusCode === '2';
            this.jobProfileSearchOption.defaultValue = this.resourceDetails.manpowerPlanJobProfileType ?
            this.resourceDetails.manpowerPlanJobProfileType.description : '';
            this.getPermissions();
            this.remainingCAFromWBS = this.resourceDetails.isRemainingCAFromWBS;
        }
        this.getResourceDates(this.resourceDetails);
        this.validateHireOnExisting();
    }

    getResourceDates(resource: any): void {
        resource.planStartDate = getDateObjectFromTimeStamp(resource.planStartDate);
        resource.planEndDate = getDateObjectFromTimeStamp(resource.planEndDate);
        resource.chargeStartDate = getDateObjectFromTimeStamp(resource.chargeStartDate);
        resource.chargeEndDate = getDateObjectFromTimeStamp(resource.chargeEndDate);
        resource.previousChargeStartDate = getDateObjectFromTimeStamp(resource.previousChargeStartDate);
        resource.previousChargeEndDate = getDateObjectFromTimeStamp(resource.previousChargeEndDate);
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    setHeadCountWarning(): void {
        if (this.resourceCategory.categoryType === 'Staff' && this.validateHeadCount()) {
            this.manpowerWarning.push('Exceed headcount please ensure there is no overlap unless approved by the funder.');
        }
    }

    selectedPerson(result: any): void {
        if (result) {
            this.resourceDetails.personId = result.personId;
            this.resourceDetails.fullName = result.fullName;
            this.resourceDetails.positionId = !['1', '3'].includes(this.upgradeOrPromotionValue) ? result.positionId : null;
            if (this.upgradeOrPromotionValue) {
                this.addStaffUpgradeComment(result);
            }
            this.setJobProfileOnResourceSearch(result);
        } else {
            if (this.previousUpgradeType && this.resourceDetails.description && this.resourceDetails.fullName) {
                this.removeStaffUpgradeComment();
            }
            this.resourceDetails.personId = this.resourceDetails.fullName = this.resourceDetails.positionId = null;
        }
    }

    addStaffUpgradeComment(person: any): void {
        const upgradeDescription = this.getUpgradeTypeDescription(this.upgradeOrPromotionValue);
        this.resourceDetails.description = this.resourceDetails.description ?
            `${this.resourceDetails.description} \n${upgradeDescription} [${person.fullName}]` :
            `${upgradeDescription} [${person.fullName}]`;
    }

    getUpgradeTypeDescription(upgradeType: string): string {
        return this.manpowerLookups.manpowerUpgradeTypes.find((type: any) => type.upgradeTypeCode === upgradeType).description;
    }

    removeStaffUpgradeComment(): void {
        const upgradeDescription = this.getUpgradeTypeDescription(this.previousUpgradeType);
        this.resourceDetails.description = this.resourceDetails.description.replace(
            `${upgradeDescription} [${this.resourceDetails.fullName}]`, '');
    }

    selectedJobProfile(result: any = null): void {
        this.resourceDetails.planJobProfileTypeCode = result ? result.jobProfileTypeCode : null;
        this.resourceDetails.manpowerPlanJobProfileType = result ? this.getSelectedJobProfileData(result) : null;
    }

    getSelectedJobProfileData(result: any): any {
        return {
            jobProfileTypeCode: result.jobProfileTypeCode,
            description: result.description
        };
    }

    setJobProfileOnResourceSearch(result: any): void {
        if (['2', '4'].includes(this.upgradeOrPromotionValue) && result.jobProfileTypeCode) {
            this.selectedJobProfile(result);
            this.clearJobProfile = new String('false');
            this.jobProfileSearchOption.defaultValue = result.jobProfileType;
            this.resourceDetails.jobProfileTypeCode = result.jobProfileTypeCode;
        }
    }

    validateResourceType(): void {
        this.map.delete('upgradeOrPromotionValue');
        if (this.resourceCategory.index === null && (this.resourceCategory.addStaffType === 'Existing') && !this.upgradeOrPromotionValue) {
            this.map.set('upgradeOrPromotionValue', '* Select a request type');
        }
    }

    validatePerson(): void {
        if (this.resourceCategory.addStaffType !== 'New' && !(this.resourceDetails.personId || this.resourceDetails.rolodexId) &&
            !this.resourceDetails.fullName) {
            this.map.set('personError', `* Enter ${this.getPersonValidationMessage()} name`);
        }
        if (!this.isResourceInSamePositionId && personDuplicationCheck(
            this.manpowerList[this.resourceCategory.categoryType], this.resourceDetails, 'personId')) {
            this.map.set('personDuplication', 'Cost Allocation for a resource cannot be more than 100% for the given duration.');
        }
        if (this.resourceCategory.categoryType !== 'Others') {
            if (this.isResourceInSamePositionId && this.positionExistsValidation(
                this.manpowerList[this.resourceCategory.categoryType], this.resourceDetails)) {
                this.map.set('personDuplication', 'This position already exists in that duration.');
            }
            if (this.isResourceInSamePositionId && this.positionId && !this.positionIdExists(this.positionId)) {
                this.existingPositionOverlap();
            }
        }
        if (this.resourceCategory.addStaffType === 'New' && this.isResourceInSamePositionId && (!this.positionId ||
            this.positionId === 'null')) {
            this.map.set('positionId', '* Select a position id');
        }
    }

    getPersonValidationMessage(): string {
        return this.resourceCategory.categoryType === 'Staff' ? 'staff' :
            this.resourceCategory.categoryType === 'Others' ? 'resource' : 'student';
    }

    costAllocationValidation(): void {
        this.limitKeypress(this.resourceDetails.costAllocation);
        if (!this.resourceDetails.costAllocation) {
            this.map.set('costAllocation', '* Enter cost allocation %');
        }
        if (this.upgradeOrPromotionValue === '4' && this.resourceDetails.costAllocation == 100) {
            // tslint:disable-next-line: max-line-length
            this.map.set('costAllocation', '* Personal allocation exceeds 100 percent in the same period in RISE records and please check the error message for the details and amend accordingly.');
        }
    }

    costAllocationPartialAllocation() {
        if (this.resourceDetails.costAllocation < 100 && !this.resourceDetails.description) {
            this.map.set('comment', '* Enter comment');
        }
        if (this.resourceDetails.costAllocation < 100 && this.remainingCAFromWBS === null) {
            this.map.set('isRemainingCAFromWBS', '* Select a value for remaining cost allocation from WBS');
        }
    }

    addResourceValidation(): boolean {
        this.dateValidation();
        this.validatePerson();
        this.validateResourceType();
        if ((this.resourceCategory.addStaffType === 'New' || (this.upgradeOrPromotionValue &&
            !['2', '4'].includes(this.upgradeOrPromotionValue))) &&
        !this.resourceDetails.planJobProfileTypeCode) {
            this.map.set('jobProfile', '* Select a planned job profile');
        }
        if (this.resourceCategory.categoryType !== 'Others') {
            this.costAllocationValidation();
        }
        if (this.resourceCategory.categoryType === 'Student') {
            if (!this.candidateTitle || this.candidateTitle === 'null') {
                this.map.set('candidateTitle', '* Select a candidate title');
            }
            this.committedCostValidation();
        }
        if (this.resourceCategory.categoryType === 'Others' && (!this.resourceType || this.resourceType === 'null')) {
            this.map.set('resourceTypes', '* Enter resource type');
        }
        if (this.resourceCategory.categoryType === 'Staff') {
            this.plannedSalaryValidations();
            this.costAllocationPartialAllocation();
        }
        return (this.map.size === 0) ? true : false;
    }

    committedCostValidation(): void {
        this.limitAmount(this.resourceDetails.committedCost, 'committedCost');
        if (!this.resourceDetails.committedCost) {
            this.map.set('committedCost', '* Enter committed cost');
        }
        if (this.resourceDetails.committedCost > this.manpowerCategory.budgetAmount) {
            this.map.set('committedCost', '* Committed amount cannot be more than the Cost Element Budget Amount.');
        }
        if (this.checkCommittedAmount(this.resourceDetails.committedCost)) {
            this.map.set('committedCost', '* Sum of committed amount should be less or equal to budget amount');
        }
    }

    plannedSalaryValidations(): void {
        this.limitAmount(this.resourceDetails.plannedBaseSalary, 'plannedBaseSalary');
        if (!this.resourceDetails.plannedBaseSalary) {
            this.map.set('plannedBaseSalary', '* Enter proposed base salary');
        }
        if (this.resourceDetails.plannedBaseSalary > this.manpowerCategory.budgetAmount) {
            this.map.set('plannedBaseSalary', '* Proposed commitment amount cannot be more than the Cost Element Budget Amount.');
        }
        if (this.resourceCategory.index !== null) {
            this.limitAmount(this.resourceDetails.plannedSalary, 'plannedSalary');
            if (this.resourceDetails.plannedSalary > this.manpowerCategory.budgetAmount) {
                this.map.set('plannedSalary', '* Proposed commitment amount cannot be more than the Cost Element Budget Amount.');
            }
        }
    }
    /**
     * @param  {any} resourceList
     * @param  {any} newResource
     * For checking the resource duplication for existing position hire scenario
     */
    positionExistsValidation(categoryList: any, newResource: any): boolean {
        let resourceArray: any = [];
        categoryList.forEach(category => {
            const duplication = category.awardManpowerResource.filter(resource => {
                const START_DATE = resource.chargeStartDate ? resource.chargeStartDate : resource.planStartDate;
                const END_DATE = resource.chargeEndDate ? resource.chargeEndDate : resource.planEndDate;
                const NEW_START_DATE = newResource.chargeStartDate ? newResource.chargeStartDate : newResource.planStartDate;
                const NEW_END_DATE = newResource.chargeEndDate ? newResource.chargeEndDate : newResource.planEndDate;
                return resource.manpowerResourceId !== newResource.manpowerResourceId &&
                resource.positionId && this.positionId && resource.positionId === this.positionId &&
                !resource.personId && !newResource.personId &&
                compareDatesForOverlap(START_DATE, END_DATE, NEW_START_DATE, NEW_END_DATE);
            });
            resourceArray = resourceArray.concat(duplication);
        });
        let value = { costAllocation: 0 };
        if (resourceArray.length) {
            value = resourceArray.reduce(function (previousValue, currentValue) {
                return { 'costAllocation': previousValue.costAllocation + currentValue.costAllocation };
            });
        }
        return (value.costAllocation + parseFloat(newResource.costAllocation)) > 100 ? true : false;
    }

    /**limitKeypress - limit the input field b/w 0 and 100 with 2 decimal points
     * @param {} value
     */
    limitKeypress(value): void {
        this.map.delete('costAllocation');
        if (validatePercentage(value)) {
            this.map.set('costAllocation', validatePercentage(value));
        }
    }

    limitAmount(value, fieldName: string): void {
        this.map.delete(fieldName);
        if (inputRestrictionForAmountField(value)) {
            this.map.set(fieldName, inputRestrictionForAmountField(value));
        }
    }

    checkCommittedAmount(value): boolean {
        const committed = this.manpowerCategory.awardManpowerResource.length ? (parseInt(value, 10) +
            calculateArraySum(this.manpowerCategory.awardManpowerResource, 'committedCost',
                this.resourceDetails.manpowerResourceId)) : parseInt(value, 10);
        return value && this.manpowerCategory.budgetAmount < committed;
    }

    capitalizeFirstLetter(msg: string): string {
        return msg.charAt(0).toUpperCase() + msg.slice(1);
    }
    /**
     * @param  {} date
     * @param  {string} valueString
     * for checking if the start date is a past date on adding a resource or editing and changing the resource start dates
     */
    compareStartDateWithSystem(date: any, valueString: string): boolean {
        const editOrNew = this.resourceCategory.index === null ? true :
            compareDates(date, this.getDateFromParent(valueString)) !== 0;
        return date && this.resourceCategory.categoryType === 'Staff' && editOrNew && compareDates(date, getSystemDate()) === -1;
    }
    /**
     * @param  {} date
     * @param  {string} valueString
     * for checking if the start date is a past date on adding a resource or editing and changing the resource start dates
     */
    staffStartDate45Days(date: any, valueString: string): boolean {
        const isEdit = this.resourceCategory.index === null ? true :
            compareDates(date, this.getDateFromParent(valueString))
            !== 0;
        return date && this.resourceCategory.addStaffType === 'New' && isEdit && getNumberOfDays(date, getSystemDate()) < 45;
    }

    getDateFromParent(valueString: string): any {
        return getDateObjectFromTimeStamp(this.manpowerCategory.awardManpowerResource[this.resourceCategory.index][valueString]);
    }
    /**
     * for validating the approved head count of a resource in staff or student category
     */
    validateHeadCount(): boolean {
        return this.resourceCategory.index === null && this.resourceCategory.categoryType !== 'Others' &&
            validateApproverHeadCount(this.manpowerCategory.approvedHeadCount,
                this.manpowerCategory.awardManpowerResource);
    }

    dateValidation(): void {
        this.map.clear();
        this.manpowerWarning = [];
        this.setHeadCountWarning();
        if (this.resourceCategory.categoryType !== 'Staff' || (this.resourceCategory.categoryType === 'Staff' &&
            (this.resourceDetails.chargeStartDate || this.resourceDetails.chargeEndDate))) {
            this.validateChargeDates();
        } else {
            this.validatePlanDates();
        }
        this.cutOffDateValidation();
        this.calculateDuration();
        this.validateHireOnExisting();
    }

    validateChargeDates(): void {
        this.validateDate(this.resourceDetails.chargeStartDate, 'chargeStartDate', this.resourceCategory.categoryType !== 'Others' ?
            'charge start date' : 'actual start date');
        this.validateDate(this.resourceDetails.chargeEndDate, 'chargeEndDate', this.resourceCategory.categoryType !== 'Others' ?
            'charge end date' : 'actual end date');
        if (this.resourceCategory.index !== null && this.resourceCategory.categoryType === 'Staff' &&
            this.resourceDetails.previousChargeEndDate && compareDates(this.resourceDetails.chargeEndDate,
                this.resourceDetails.previousChargeEndDate) === 1) {
            this.map.set('chargeEndDate', '* Charge end date cannot be extended');
        }
        if (this.resourceCategory.index !== null && this.resourceCategory.categoryType === 'Staff' &&
            this.resourceDetails.previousChargeStartDate && compareDates(this.resourceDetails.chargeStartDate,
                this.resourceDetails.previousChargeStartDate) === -1) {
            this.map.set('chargeStartDate', '* Charge start date cannot be reduced');
        }
        if (this.resourceDetails.chargeStartDate && this.resourceDetails.chargeEndDate &&
            compareDates(this.resourceDetails.chargeStartDate, this.resourceDetails.chargeEndDate) === 1) {
            this.map.set('chargeEndDate', '* Enter a charge end date after charge start date');
        }
    }

    validatePlanDates(): void {
        this.validateDate(this.resourceDetails.planStartDate, 'planStartDate', 'plan start date');
        this.validateDate(this.resourceDetails.planEndDate, 'planEndDate', 'plan end date');
        if (this.resourceDetails.planStartDate && this.resourceDetails.planEndDate &&
            compareDates(this.resourceDetails.planStartDate, this.resourceDetails.planEndDate) === 1) {
            this.map.set('planEndDate', '* Enter a plan end date after plan start date');
        }
    }
    /**
     * cutoff date is checked to restrict any resource to be added after cutoff date.
     */
    cutOffDateValidation(): void {
        if (this.resourceCategory.categoryType === 'Staff' && checkCutoffDate(this.resourceDetails.chargeStartDate ?
            this.resourceDetails.chargeStartDate : this.resourceDetails.planStartDate, this.manpowerLookups.manpowerCutOffDate)) {
            this.map.set('cutOffValidation', 'This resource with the current charge start date overlaps with the cut-off date');
        }
    }
    /**
     * @param  {} date date to be validated
     * @param  {string} mapValue value in which the map has to set
     * @param  {string} msgVariable message which has to be displayed in the validation
     * function validates a date which is passed
     */
    validateDate(date: any, mapValue: string, msgVariable: string): void {
        if (!date) {
            this.map.set(mapValue, '* Enter ' + msgVariable);
        } else {
            this.compareWithAwardDates(date, mapValue, msgVariable);
            this.pastDateValidation(date, mapValue, msgVariable);
            this.startDate45DaysWarning(date, mapValue, msgVariable);
        }
    }

    compareWithAwardDates(date: any, mapValue: string, msgVariable: string): void {
        const AWARDSTARTDATE = getDateObjectFromTimeStamp(this._commonData.beginDate);
        const AWARDENDDATE = getDateObjectFromTimeStamp(this._commonData.finalExpirationDate);
        if (compareDates(date, AWARDSTARTDATE) === -1) {
            if (this.resourceCategory.categoryType === 'Student') {
                this.manpowerWarning.push(`Enter a valid ${this.upperCaseFirstLetters(msgVariable)} after Award Start Date`);
            } else {
                this.map.set(mapValue, '* Enter a valid ' + msgVariable + '  after award begin date');
            }
        }
        if (compareDates(date, AWARDENDDATE) === 1) {
            if (this.resourceCategory.categoryType === 'Student') {
                this.manpowerWarning.push(`Enter a valid ${this.upperCaseFirstLetters(msgVariable)} before Award End Date`);
            } else {
                this.map.set(mapValue, '* Enter a valid ' + msgVariable + ' before award expiration date');
            }
        }
    }
    /**
     * @param  {any} date
     * @param  {string} mapValue
     * @param  {string} msgVariable
     * compares system date and blocks the date if user selects a past date
     */
    pastDateValidation(date: any, mapValue: string, msgVariable: string): void {
        if (((!this.canModifyChargeEndDate && ['planEndDate', 'chargeEndDate'].includes(mapValue)) ||
            ['planStartDate', 'chargeStartDate'].includes(mapValue)) && this.compareStartDateWithSystem(date, mapValue)) {
            this.map.set(mapValue, '* ' + this.capitalizeFirstLetter(msgVariable) + ' cannot be a past date.');
        }
    }
    /**
     * @param  {any} date
     * @param  {string} mapValue
     * @param  {string} msgVariable
     * warning message shown when the start date selected for staff is not at least 45 days from the system date
     */
    startDate45DaysWarning(date: any, mapValue: string, msgVariable: string): void {
        if (mapValue === 'planStartDate' && !this.map.has(mapValue) && this.staffStartDate45Days(date, mapValue)) {
            this.manpowerWarning.push(this.capitalizeFirstLetter(msgVariable) +
                ' is less than 45 days as hiring process may not be completed in time.');
        }
    }
    /**
     * @param  {string} string
     * converts every 1st letter in the sentence to upper case
     */
    upperCaseFirstLetters(string: string): string {
        return string.replace(/(^\w{1})|(\s{1}\w{1})/g, match => match.toUpperCase());
    }

    calculateDuration(): void {
        let durationObject: any;
        if (this.resourceDetails.planStartDate && this.resourceDetails.planEndDate) {
            durationObject = getDuration(this.resourceDetails.planStartDate, this.resourceDetails.planEndDate, true);
            this.resourceDetails.planDuration = durationObject.durInYears + ' year(s), ' +
                durationObject.durInMonths + ' month(s) & ' + durationObject.durInDays + ' day(s)';
        }
        if (this.resourceDetails.chargeStartDate && this.resourceDetails.chargeEndDate) {
            durationObject = getDuration(this.resourceDetails.chargeStartDate, this.resourceDetails.chargeEndDate, true);
            this.resourceDetails.chargeDuration = durationObject.durInYears + ' year(s), ' +
                durationObject.durInMonths + ' month(s) & ' + durationObject.durInDays + ' day(s)';
        }
    }
    /**
     * for setting elastic options for the search
     */
    changeResourceType(): void {
        this.resourceDetails.personId = this.resourceDetails.fullName =
            this.resourceDetails.rolodexId = null;
        this.employeeSearchOption.defaultValue = '';
        (this.isEmployeeFlag) ? this.setElasticPersonOption() : this.setElasticRolodexOption();
    }
    /**setElasticPersonOption - Set Elastic search option for Fibi Person */
    setElasticPersonOption(): void {
        this.employeeSearchOption = this._elasticConfig.getElasticForPerson();
    }
    /**setElasticRolodexOption - Set Elastic search option for Fibi rolodex */
    setElasticRolodexOption(): void {
        this.employeeSearchOption = this._elasticConfig.getElasticForRolodex();
    }
    /**
     * @param  {} result
     * for selecting the elastic result
     */
    selectedEmployee(result: any): void {
        if (result) {
            this.resourceDetails.personId = result.prncpl_id;
            this.resourceDetails.fullName = result.full_name;
            this.resourceDetails.rolodexId = result.rolodex_id;
        } else {
            this.resourceDetails.personId = this.resourceDetails.fullName =
                this.resourceDetails.rolodexId = null;
        }
    }
    /**
     * add or update resource details. This function emits the data to the main component where the service is called
     */
    addResource(): void {
        if (this.addResourceValidation()) {
            if (!this.resourceDetails.manpowerResourceId && this.resourceCategory.categoryType === 'Student') {
                this.resourceDetails.candidateTitleTypeCode = this.candidateTitle;
                this.resourceDetails.manpowerCandidateTitleType = this.getManpowerCandidateTitleType();
            }
            if (this.resourceCategory.categoryType === 'Staff') {
                this.resourceDetails.positionId = this.getPositionIdValue();
                this.resourceDetails.isRemainingCAFromWBS = this.remainingCAFromWBS;
            }
            if (this.resourceCategory.categoryType === 'Others') {
                this.resourceDetails.resourceTypeCode = this.resourceType;
                this.resourceDetails.manpowerResourceType = this.getManpowerResourceType();
            }
            this.resourceDetails.awardManpowerId = this.manpowerCategory.awardManpowerId;
            this.resourceDetails.upgradeTypeCode = this.upgradeOrPromotionValue;
            this.resourceDetails.manpowerUpgradeType = this.getUpgradeType();
            this.saveOrUpdateManpowerResource();
        }
    }

    getManpowerCandidateTitleType(): any {
        return this.candidateTitle ? this.manpowerLookups.manpowerCandidateTitleType.find(
            (type: any) => type.candidateTitleTypeCode === this.candidateTitle) : null;
    }

    getManpowerResourceType(): any {
        return this.resourceType ? this.manpowerLookups.manpowerResourceType.find(
            (type: any) => type.resourceTypeCode === this.resourceType) : null;
    }

    getUpgradeType(): any {
        return this.upgradeOrPromotionValue ? this.manpowerLookups.manpowerUpgradeTypes.find(
            (type: any) => type.upgradeTypeCode === this.upgradeOrPromotionValue) : null;
    }

    getPositionIdValue(): string {
        const isPositionGenerated = this.resourceCategory.index !== null ?
            this.resourceDetails.manpowerPositionStatus.positionStatusCode !== '3' : true;
        return this.resourceCategory.addStaffType === 'New' && isPositionGenerated ? this.positionId : this.resourceDetails.positionId;
    }
    /**
     * set the required details for adding or updating a resource
     */
    setRequestObject(): void {
        this.resourceDetails.createUser = this._commonService.getCurrentUserDetail('userName');
        this.resourceDetails.updateUser = this._commonService.getCurrentUserDetail('userName');
        this.resourceDetails.planStartDate = parseDateWithoutTimestamp(this.resourceDetails.planStartDate);
        this.resourceDetails.planEndDate = parseDateWithoutTimestamp(this.resourceDetails.planEndDate);
        this.resourceDetails.chargeStartDate = parseDateWithoutTimestamp(this.resourceDetails.chargeStartDate);
        this.resourceDetails.chargeEndDate = parseDateWithoutTimestamp(this.resourceDetails.chargeEndDate);
        this.resourceDetails.previousChargeStartDate = parseDateWithoutTimestamp(this.resourceDetails.previousChargeStartDate);
        this.resourceDetails.previousChargeEndDate = parseDateWithoutTimestamp(this.resourceDetails.previousChargeEndDate);
        this.resourceDetails.plannedSalary = convertToValidAmount(this.resourceDetails.plannedSalary);
        this.resourceDetails.committedCost = convertToValidAmount(this.resourceDetails.committedCost);
        this.resourceDetails.plannedBaseSalary = convertToValidAmount(this.resourceDetails.plannedBaseSalary);
    }

    setSaveRequestObject(): any {
        return {
            'addManpowerCategoryType': this.resourceCategory.addStaffType,
            'awardId': this.awardData.award.awardId,
            'awardNumber': this.awardData.award.awardNumber,
            'budgetAmount': this.manpowerCategory.budgetAmount,
            'isReadyToHire': this.resourceCategory.categoryType === 'Staff' ? this.readyToHire(this.resourceCategory.addStaffType) : null,
            'awardManpowerResource': this.resourceDetails,
            'sequenceNumber': this.awardData.award.sequenceNumber,
            'isUpdateInitialCommittedAmount': this.resourceCategory.index !== null && (this.resourceDetails.plannedSalary !==
                this.manpowerCategory.awardManpowerResource[this.resourceCategory.index].plannedSalary),
            'isBaseSalaryFieldValuesChanged': this.resourceCategory.index === null ? true : this.isChangeInCalculationRelatedFields,
            'isHiringOnExistingPosition': this.isResourceInSamePositionId
        };
    }

    saveManpowerResponse(data: any): void {
        this.getResourceDates(this.resourceDetails);
        if (data.isCostAllocationValidationExist) {
            // tslint:disable-next-line: max-line-length
            this.isCostAllocationValidation = 'Cost Allocation for a resource cannot be more than 100% (active award(s) + current award). List of awards in which this resource is used';
            this.costAllocationList = data.awardManpowerResources;
        }
        if (!data.isCostAllocationValidationExist) {
            this.resourceAddSuccess(this.resourceCategory.index, data);
        }
    }
    /**
     * service to add or update a resource
     */
    saveOrUpdateManpowerResource(): void {
        if (!this.isSaving) {
            this.isSaving = true;
            if (this.resourceCategory.index !== null) {
                this.detectChangeInCalculationRelatedFields(this.manpowerCategory.awardManpowerResource[this.resourceCategory.index]);
            }
            this.setRequestObject();
            this.$subscriptions.push(this._manpowerService.saveOrUpdateManpowerResource(
                this.setSaveRequestObject()).subscribe((data: any) => {
                    this.manpowerLookups.positionIds = data.positionIds ? data.positionIds : this.manpowerLookups.positionIds;
                    this.saveManpowerResponse(data);
                    this.isSaving = false;
                }, err => {
                    this.resourceCategory.index !== null ? this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating resources failed. Please try again') :
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Adding resources failed. Please try again.');
                    $('#addManpowerResource').modal('hide');
                    this.resourceOperations.emit(true);
                    this.isSaving = false;
                }));
        }
    }

    readyToHire(staffType: string): boolean {
        const isPositionGenerated = this.resourceCategory.index !== null &&
            this.resourceDetails.manpowerPositionStatus.positionStatusCode === '3';
        const isUseReadyToHireFlag = this.resourceCategory.index !== null &&
            !['1', '2', '7'].includes(this.resourceDetails.manpowerPositionStatus.positionStatusCode);
        return ((staffType === 'Existing' && ['1', '3'].includes(this.upgradeOrPromotionValue) && !isUseReadyToHireFlag) ||
                (staffType === 'New' && !this.isResourceInSamePositionId))
            && !isPositionGenerated ? this.isReadyToHire : null;
    }
    /**
     * @param  {} editedObject the resource which is being edited
     * checks if any changes are made in the fields which affect the calculation of initial committed amount
     */
    detectChangeInCalculationRelatedFields(editedObject: any): void {
        const PLAN_START_DATE = getDateObjectFromTimeStamp(editedObject.planStartDate);
        const PLAN_END_DATE = getDateObjectFromTimeStamp(editedObject.planEndDate);
        const CHARGE_START_DATE = getDateObjectFromTimeStamp(editedObject.chargeStartDate);
        const CHARGE_END_DATE = getDateObjectFromTimeStamp(editedObject.chargeEndDate);
        this.isChangeInCalculationRelatedFields = (this.resourceDetails.plannedBaseSalary !== editedObject.plannedBaseSalary ||
            this.resourceDetails.costAllocation !== editedObject.costAllocation ||
            compareDates(this.resourceDetails.planStartDate, PLAN_START_DATE) !== 0 ||
            compareDates(this.resourceDetails.chargeStartDate, CHARGE_START_DATE) !== 0 ||
            compareDates(this.resourceDetails.planEndDate, PLAN_END_DATE) !== 0 ||
            compareDates(this.resourceDetails.chargeEndDate, CHARGE_END_DATE) !== 0);
    }

    /**
     * @param  {} index
     * @param  {} data
     * used for updating the data on the success of the service call.
     */
    resourceAddSuccess(index: number, data: any): void {
        index !== null ? updateEditedManpowerResource(this.manpowerCategory, index, data.awardManpowerResource) :
            addResourceToList(this.manpowerCategory, data.awardManpowerResource);
        if (data.awardManpowerDetail) {
            this.manpowerCategory.actualHeadCount = data.awardManpowerDetail.actualHeadCount;
            this.manpowerCategory.sapCommittedAmount = data.awardManpowerDetail.sapCommittedAmount;
            if (this.resourceCategory.categoryType === 'Staff') {
                this.manpowerCategory.uncommittedAmount = calculateUncommittedAmount(
                    this.manpowerCategory.awardManpowerResource, this.manpowerCategory.budgetAmount,
                    data.awardManpowerDetail.sapCommittedAmount);
            }
        }
        this.resourceCategory.index !== null ? this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Resource successfully updated.') :
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Resource successfully added.');
        $('#addManpowerResource').modal('hide');
        this.resourceOperations.emit(true);
    }
    /**
     * @param  {} rolodexObject
     * setting the value for rolodex when a new rolodex is added
     */
    setRolodexPersonObject(rolodexObject: any): void {
        if (rolodexObject.rolodex) {
            (this.isEmployeeFlag) ? this.setElasticPersonOption() : this.setElasticRolodexOption();
            this.resourceDetails.fullName = rolodexObject.rolodex.fullName;
            this.resourceDetails.rolodexId = rolodexObject.rolodex.rolodexId;
            this.employeeSearchOption.defaultValue = this.resourceDetails.fullName;
            this.map.delete('personError');
        }
        this.isAddNonEmployeeModal = rolodexObject.nonEmployeeFlag;
    }
    /**
     * @param  {boolean} condition
     * sets the required condition for upgrade or promotion case
     */
    toggleUpgradeOrPromotion(): void {
        if (this.upgradeOrPromotionValue === 'null') {
            this.upgradeOrPromotionValue = null;
        }
        this.map.delete('personError');
        this.map.delete('jobProfile');
        this.selectedPerson(null);
        this.isResourceInSamePositionId = false;
        this.isReadyToHire = false;
        this.positionId = null;
        this.remainingCAFromWBS = null;
        this.selectedJobProfile(null);
        this.clearJobProfile = new String('true');
        this.jobProfileSearchOption.defaultValue = '';
        this.resourceDetails.costAllocation = this.upgradeOrPromotionValue && this.upgradeOrPromotionValue !== '4' ? '100' : '';
        this.setResourceSearchOption();
        this.previousUpgradeType = this.upgradeOrPromotionValue;
    }
    /**
     * To calculate the initial committed cost
     */
    calculatePlannedSalary(): void {
        if (this.addResourceValidation()) {
            this.setRequestObject();
            this.$subscriptions.push(this._manpowerService.calculatePlannedSalary({
                'awardManpowerResource': this.resourceDetails, 'awardId': this.awardData.award.awardId
            }).subscribe((data: any) => {
                this.resourceDetails.plannedSalary = data ? data.validatedPlannedAmount : this.resourceDetails.plannedSalary;
                this.getResourceDates(this.resourceDetails);
            }));
        }
    }
    /**
     * For validating the duration overlapping for hiring in existing position
     */
    validateHireOnExisting(): void {
        this.clearHireOnExistingWarning();
        if (this.isResourceInSamePositionId && this.positionId && checkDuplicationExistingHire(
            this.manpowerList[this.resourceCategory.categoryType], this.resourceDetails, this.positionId)) {
            this.manpowerWarning.push('The duration for hiring on existing position is overlapping');
        }
    }

    clearHireOnExistingWarning(): void {
        if (this.manpowerWarning.find(element => element === 'The duration for hiring on existing position is overlapping')) {
            this.manpowerWarning.splice(this.manpowerWarning.indexOf('The duration for hiring on existing position is overlapping'), 1);
        }
    }

    positionIdExists(positionId: any): boolean {
        return this.manpowerCategory.awardManpowerResource.find(resource => resource.positionId && resource.personId
            && resource.positionId === positionId) ? true : false;
    }

    existingPositionOverlap(): void {
        if (newExistingPositionOverlap(this.manpowerList[this.resourceCategory.categoryType],
            this.resourceDetails, this.positionId, this.manpowerCategory.budgetReferenceNumber)) {
            this.map.set('personDuplication', 'This position id overlaps the duration of another resource.');
        }
    }

}
