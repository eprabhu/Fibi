import { Component, EventEmitter, Input, OnInit, Output,} from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subject, Subscription } from 'rxjs';
import { DATE_PLACEHOLDER } from '../../../src/app/app-constants';
import { getEndPointOptionsForCountry } from '../../../../fibi/src/app/common/services/end-point.config';
import { deepCloneObject, isEmptyObject, openModal } from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { environment } from '../../environments/environment';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../app-constants';
import { CommonService } from '../common/services/common.service';
import { NavigationService } from '../common/services/navigation.service';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { ElasticConfigService } from '../common/services/elastic-config.service';
import { setEntityObjectFromElasticResult } from '../common/utilities/elastic-utilities';
import { InformationAndHelpTextService } from '../common/services/informationAndHelpText.service';
import { compareDates, parseDateWithoutTimestamp } from '../common/utilities/date-utilities';


declare const $: any;
export interface EndpointOptions {
    contextField: string;
    formatString: string;
    path: string;
    defaultValue: string;
    params: string;
}
@Component({
    selector: 'app-add-sfi',
    templateUrl: './add-sfi.component.html',
    styleUrls: ['./add-sfi.component.scss']
})
export class AddSfiComponent implements OnInit {
    isSaving = false;
    // entityDetails: EntityDetails = new EntityDetails();
    entityDetails: any = {};
    additionalDetails: any = {
        sponsorsResearch: false
    };
    deployMap = environment.deployUrl;
    isAddAttachment = false;
    isAddAssignee = false;
    dateTime: string;
    datePlaceHolder = DATE_PLACEHOLDER;
    isReadMore: false;
    clearField: any = false;
    EntitySearchOptions: any = {};
    $subscriptions: Subscription[] = [];
    mandatoryList = new Map();
    emailWarningMsg: any;
    sfiLookUpList: any = {};
    isExpandedAdditionalDetails = true;
    isResultFromSearch = false;
    riskLevelLookup = [];
    heading: string;
    buttonName: string;
    btnTitle = '';
    isViewMode: any;
    sfiType: string;
    existingEntityDetails: any = {};
    canShowEntityFields = false;
    addEntityConfirmation: any = null;
    isAddressReadMore: false;
    isChecked = {};
    relationLookup: any = [];
    concurrencyPersonEntityId = null;
    involvementDate =  {
        involvementStartDate: null,
        involvementEndDate: null
    }
    isNewEntityFromSearch = false;
    saveEntity = new Subject();
    initalProceed = new Subject();

    @Output() emitUpdateEvent = new EventEmitter<number>();
    @Input() modifyType = '';
    @Input() disclosureDetails: { disclosureId: any, disclosureNumber: any } = { disclosureId: null, disclosureNumber: null };
    @Input() coiEntityManageId: any = null;
    @Input() isEditEntity = false;
    @Input() isSlider = false;
    @Input() revisionReason = '';
    @Input() sfiSliderSectionConfig: any;
    @Input() entitySectionConfig: any;

    constructor(public sfiService: SfiService, private _activatedRoute: ActivatedRoute,
        public _commonService: CommonService, private _router: Router, public _navigationService: NavigationService,
        private _elasticConfig: ElasticConfigService, private _informationAndHelpTextService: InformationAndHelpTextService) { }

    ngOnInit(): void {
        this.getSfiSliderSectionConfig();
        this.setHeader();
        this.getRelationshipLookUp();
        this.EntitySearchOptions = this._elasticConfig.getElasticForActiveEntity();
        window.scrollTo(0,0);
    }

    private checkIfSFIAlreadyAdded(entityId, event): void {
        this.mandatoryList.delete('entityAlreadyAdded');
        this.addEntityConfirmation = null;
        this.$subscriptions.push(this.sfiService.isEntityAdded(entityId).subscribe((res: any) => {
            if (res) {
                this.existingEntityDetails = res;
                if (this.existingEntityDetails.personEntityRelationships.length) {
                    this.existingEntityDetails.personEntityRelationships = this.groupByDisclosureType(deepCloneObject( this.existingEntityDetails.personEntityRelationships), "coiDisclosureType", "description", "validPersonEntityRelType");
                } else {
                    this.existingEntityDetails.personEntityRelationships = {};
                }
                this.mandatoryList.set('entityAlreadyAdded', 'An SFI has already been created against the entity you are trying to add. To view the SFI, please click on the View button on the SFI card.');
            } else {
                openModal('entity-details');
                this.addEntityConfirmation = event;
            }
        }, err => {
            this.EntitySearchOptions = this._elasticConfig.getElasticForActiveEntity();
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Entity selection failed. Please try again');
        }));
    }

    setDateValues() {
        this.additionalDetails.involvementStartDate = this.involvementDate.involvementStartDate ? parseDateWithoutTimestamp(this.involvementDate.involvementStartDate) : '';
        this.additionalDetails.involvementEndDate = this.involvementDate.involvementEndDate ? parseDateWithoutTimestamp(this.involvementDate.involvementEndDate) : '';
    }

    private saveAdditionalDetails(): void {
        if(!this.mandatoryList.size) {
            this.isSaving = true;
            this.$subscriptions.push(this.sfiService.createSFI(
                {
                    entityId: this.entityDetails.entityId,
                    ...this.additionalDetails,
                    "validPersonEntityRelTypeCodes": this.getSelectedRelationTypeCodes().map(typeCode => Number(typeCode))
                }).subscribe((data: any) => {
                    if (data) {
                        this.additionalDetails = data.personEntity;
                        this.isSaving = false;
                        this.navigateToSFI(data.personEntityId);
                        this.isNewEntityFromSearch = false;
                    }
                    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'SFI saved successfully.');
                }, _err => {
                    this.isSaving = false;
                        if (_err.status === 405) {
                            this.concurrencyPersonEntityId = _err.error.personEntityId;
                            openModal('coi-add-sfi-concurrency-modal');
                        } else {
                            this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving SFI. Please try again.');
                        }
                }));
        }
    }

    validateRelationship(elementIdList) {
        if (!this.getSelectedRelationTypeCodes().length) {
            this.mandatoryList.set('relationRadio', 'Please select atleast one relationship.');
            elementIdList.push('relation-radio-btn');
        }
    }

    checkForNotEmpty(val) {
       return !isEmptyObject(val);
    }

    selectedEvent(event): void {
        this.canShowEntityFields = false;
        this.clearSFIFields();
        if (event) {
            this.clearField = new String('false');
            event = setEntityObjectFromElasticResult(event);
            this.checkIfSFIAlreadyAdded(event.entityNumber, event);
        } else {
            this.sfiService.$addRelationService.next(null);
        }
    }

    private clearSFIFields(): void {
        this.entityDetails = {};
        this.additionalDetails = {
            sponsorsResearch: false
        };
        this.clearDates();
        this.isChecked = {};
        this.isResultFromSearch = false;
        this.mandatoryList.clear();
        this.isNewEntityFromSearch = false;
    }

    clearDates() {
        this.involvementDate = {
            involvementStartDate: null,
            involvementEndDate: null
        }
    }

    private checkMandatoryFilled(): boolean {
        this.mandatoryList.clear();
        const ELEMENT_ID_LIST = [];
        if (!this.canShowEntityFields && !this.entityDetails?.entityName) {
            this.mandatoryList.set('entityName', 'Please enter Entity Name.');
            ELEMENT_ID_LIST.push('coi-add-sfi-entity-name');
        }
        if (!this.involvementDate.involvementStartDate) {
            this.mandatoryList.set('date', 'Please enter Start Date.');
            ELEMENT_ID_LIST.push('coi-add-sfi-start-date')
        }
        this.endDateValidation(ELEMENT_ID_LIST);
        this.validateRelationship(ELEMENT_ID_LIST);
        if (!this.additionalDetails.staffInvolvement) {
            this.mandatoryList.set('staff', 'Please enter Relationship with Entity.');
            ELEMENT_ID_LIST.push('coi-add-sfi-releationship')
        }
        if (!this.additionalDetails.studentInvolvement) {
            this.mandatoryList.set('student', 'Please enter Principle Business Area of Entity.');
            ELEMENT_ID_LIST.push('coi-add-sfi-business-area')
        }
        if (!this.additionalDetails.instituteResourceInvolvement) {
            this.mandatoryList.set('resource', 'Please enter Relationship of Entity to your University responsibilities.');
            ELEMENT_ID_LIST.push('coi-add-sfi-resource-sfi');
        }
        this.focusValidationField(ELEMENT_ID_LIST)
        return this.mandatoryList.size !== 0 ? false : true;
    }

    endDateValidation(elementIdList = []): void {
        this.mandatoryList.delete('endDate');
        if (this.involvementDate.involvementStartDate && this.involvementDate.involvementEndDate &&
            (compareDates(this.involvementDate.involvementStartDate, this.involvementDate.involvementEndDate) === 1)) {
            this.mandatoryList.set('endDate', 'Please provide a valid end date.');
            elementIdList.push('end-date-involvement')
        }
    }

    onDateSelect() {
        this.endDateValidation();
        this.setDateValues();
    }

    setHeader(): void {
        this.heading = 'Significant Financial Interest';
        this.buttonName = 'Save';
        this.btnTitle = 'Click here to save SFI';
    }

    submitEntity(): void {
        if (this.mandatoryList.has('entityAlreadyAdded')){
            return;
        }
        this.checkMandatoryFilled();
        if(!this.mandatoryList.size) {
            this.entityDetails.entityId ? this.saveAdditionalDetails() : this.saveEntity.next(true);
        } else {
            this.initalProceed.next(true);
        }
    }

    backToPreviousPage(): void {
        if (this._navigationService.previousURL) {
            this._router.navigateByUrl(this._navigationService.previousURL);
        } else {
            this._router.navigate(['/coi/user-dashboard']);
        }
    }

    viewSfiDetails() {
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: this.existingEntityDetails.personEntityId,personEntityNumber: this.existingEntityDetails.entityNumber } });
    }

    editSfiDetails(personEntityId) {
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: personEntityId, mode: 'E', personEntityNumber: this.existingEntityDetails.entityNumber } });
      }

    viewEntityDetails(event) {
        this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: event } });
    }

    confirmEntityDetails() {
        this.isResultFromSearch = true;
        this.entityDetails.entityName = this.addEntityConfirmation.entityName;
        this.entityDetails.entityId = this.addEntityConfirmation.entityId;
    }

    clearEntityDetails() {
        this.clearField = new String('true');
        this.addEntityConfirmation = null;
        this.canShowEntityFields = false;
    }

    getSelectedRelationTypeCodes() {
        return Object.keys(this.isChecked).filter(key => this.isChecked[key]);
    }

    getRelationshipLookUp() {
        this.$subscriptions.push(this.sfiService.addSFILookUp().subscribe((res: any) => {
            if(res) {
                this.relationLookup = this.groupBy(res.validPersonEntityRelTypes, "coiDisclosureType", "description");
            }
        }));
    }

    groupBy(jsonData, key, innerKey) {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[key][innerKey]] = relationsTypeGroup[item[key][innerKey]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

    groupByDisclosureType(jsonData: any, coiDisclosureType: any, description: any, validPersonEntityRelType: any): {} {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[validPersonEntityRelType][coiDisclosureType][description]] = relationsTypeGroup[item[validPersonEntityRelType][coiDisclosureType][description]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

    navigateToSFI(personEntityId) {
        this.sfiService.isShowSfiNavBar = false;
        document.body.removeAttribute("style");
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: personEntityId, personEntityNumber: this.existingEntityDetails.entityNumber } });
    }

    goToHome() {
        this._router.navigate(['/coi/user-dashboard']);
    }

    navigateBack() {
        this._router.navigateByUrl(this._navigationService.previousURL);
    }

    focusValidationField(elementList) {
        if(elementList.length) {
            const ELEMENT: HTMLElement = document.getElementById(elementList[0]);
            const OFFSET_HEADER = document.getElementById('create-sfi-header')?.clientHeight;
            const SECTION_HEIGHT = ELEMENT.offsetTop - OFFSET_HEADER;
            if (document.activeElement.id != elementList[0]) {
                this.isSlider ? document.getElementById('add-sfi').scrollTo({ behavior: 'smooth', top: SECTION_HEIGHT }) :window.scrollTo({ behavior: 'smooth', top: SECTION_HEIGHT });
            }
            ELEMENT.focus();
        }
    }

    addNewEntity(event: string) {
        this.clearSFIFields();
        this.isNewEntityFromSearch = true;
        this.entityDetails.entityName = event;
        this.canShowEntityFields = true;
    }

    getSfiSliderSectionConfig(){
        let SFI_CONFIG =  this._commonService.getSectionCodeAsKeys(
            this.isSlider ? this.sfiSliderSectionConfig : this._activatedRoute.snapshot.data.moduleConfig
        );
        let ENTITY_CONFIG =  this._commonService.getSectionCodeAsKeys(this.isSlider ? this.entitySectionConfig :
            this._activatedRoute.snapshot.data.entityConfig);
        this._informationAndHelpTextService.moduleConfiguration = {...SFI_CONFIG, ...ENTITY_CONFIG};
    }

    getIcon(key): string {
        switch(key) {
            case 'Commitment': return 'handshake';
            case 'Travel': return 'flight';
            case 'Financial': return 'paid';
            case 'Consulting' : return 'supervisor_account';
            default: return;
        }
    }

    setEntityId(event) {
        this.entityDetails.entityId = event.entityId;
        this.entityDetails.entityNumber = event.entityNumber;
        this.entityDetails.entityName = event.entityName;
        this.saveAdditionalDetails()
    }
}
