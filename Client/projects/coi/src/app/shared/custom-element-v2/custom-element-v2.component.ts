import { Component, OnInit, Input, OnDestroy, Output, EventEmitter } from '@angular/core';
import { CustomElementService } from '../custom-element/custom-element.service';
import { CommonService } from '../../common/services/common.service';
import { Observable, Subscription } from 'rxjs';
import { setFocusToElement } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { parseDateWithoutTimestamp } from '../../../../../fibi/src/app/common/utilities/date-utilities';
import { ElasticConfigService } from '../../../../../fibi/src/app/common/services/elastic-config.service';
import {
    getEndPointOptionsForCostCentre,
    getEndPointOptionsForCountry,
    getEndPointOptionsForDepartment, getEndPointOptionsForFundCentre, getEndPointOptionsForGrandCode,
    getEndPointOptionsForLeadUnit,
    getEndPointOptionsForMappedClaimTemplate,
    getEndPointOptionsForOrganization,
    getEndPointOptionsForProfitCentre,
    getEndPointOptionsForSponsor
} from '../../../../../fibi/src/app/common/services/end-point.config';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { AutoSaveService } from '../../../../../fibi/src/app/common/services/auto-save.service';
import { DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../../fibi/src/app/app-constants';

@Component({
    selector: 'app-custom-element-v2',
    templateUrl: './custom-element-v2.component.html',
    styleUrls: ['./custom-element-v2.component.scss']
})
export class CustomElementV2Component implements OnInit, OnInit, OnDestroy {

    @Input() moduleItemKey;
    @Input() moduleCode;
    @Input() viewMode;
    @Input() customElement;
    @Output() dataChangeEvent = new EventEmitter<boolean>();
    @Input() externalSaveEvent: Observable<any>;
    @Input() isShowSave = true;
    @Input() isShowCollapse = false;
    customElements: any = [];
    result: any = {};
    isLength = false;
    isType = false;
    isEmpty = false;
    isCheckBoxEmpty = false;
    isRadioEmpty = false;
    radioEmptyFlag;
    checkEmptyFlag;
    isValueEmpty: any = [];
    checkEmpty: any = [];
    radioEmpty: any = [];
    validationId: any = [];
    lengthValidationId: number;
    numberValidationId: number;
    datePlaceHolder = DEFAULT_DATE_FORMAT;
    $subscriptions: Subscription[] = [];
    ESOptions: any = {};
    EPOptions: any = {};
    setFocusToElement = setFocusToElement;
    parseDateWithoutTimestamp = parseDateWithoutTimestamp;
    isSaving = false;
    isDataChange = false;
    isShowOtherInfo = true;
    collapseViewMore = {};

    searchObjectMapping = {
        'fibiperson': 'prncpl_id',
        'awardfibi': 'award_number',
        'fibiproposal': 'proposal_id',
        'instituteproposal': 'proposal_id',
        'grantcall_elastic': 'grant_header_id',
        'sponsorName': 'sponsorCode',
        'unitName': 'unitNumber',
        'fibiOrganization': 'organizationId',
        'fibiCountry': 'countryCode',
        'fibiDepartment': 'unitNumber',
        'grantCodeName': 'grantCode',
        'costCenterName': 'costCenterCode',
        'fundCenterName': 'fundCenterCode',
        'profitCenterName': 'profitCenterCode',
        'claimTemplateName': 'claimTemplateCode'
    };

    constructor(private _customService: CustomElementService, public _commonService: CommonService,
        private _elasticConfig: ElasticConfigService) { }

    ngOnInit() {
        this.$subscriptions.push(this._customService.getCustomData(this.moduleCode, this.moduleItemKey)
            .subscribe(data => {
                this.result = data || [];
                if (this.result) {
                    this.customElements = this.result.customElements;
                    this.setDefaultValues(this.customElements);
                }
            }));
        this.autoSaveEvent();
    }

    collapseViewMoreOption(id: number, flag: boolean): void {
        this.collapseViewMore[id] = !flag;
    }

    /**
   * @param  {} customElementList
   * sets the default value if any based on fieldType.
   */
    setDefaultValues(customElementList) {
        customElementList.forEach(element => {
            switch (element.filterType) {
                case 'Elastic Search': this.setElasticOptions(element); break;
                case 'Autosuggest': this.setEndpointOptions(element); break;
                default: element.answers.findIndex(item => item.value = item.value ? item.value : element.defaultValue);
            }
        });
    }

    /**
   * this Event subscribes to the auto save trigger generated on save click on top basically
   * what happens is when a save click happen this will let this component know when
   * user click the general save button.
   */
    autoSaveEvent() {
        if (this.externalSaveEvent) {
            this.$subscriptions.push(this.externalSaveEvent.subscribe(_event => this.isDataChange && this.saveCustomData()));
        }
    }

    setElasticOptions(object) {
        switch (object.lookupArgument) {
            case 'fibiproposal': this.ESOptions[object.columnName] = this._elasticConfig.getElasticForProposal(); break;
            case 'fibiperson': this.ESOptions[object.columnName] = this._elasticConfig.getElasticForPerson(); break;
            case 'awardfibi': this.ESOptions[object.columnName] = this._elasticConfig.getElasticForAward(); break;
            case 'instituteproposal': this.ESOptions[object.columnName] = this._elasticConfig.getElasticForProposal(); break;
            case 'grantcall_elastic': this.ESOptions[object.columnName] = this._elasticConfig.getElasticForGrantCall(); break;
            default: break;
        }
        this.ESOptions[object.columnName].defaultValue = object.answers[0].description ? object.answers[0].description : '';
        this.ESOptions[object.columnName].contextField = object.defaultValue || this.ESOptions[object.columnName].contextField;
    }

    setEndpointOptions(object) {
        switch (object.lookupArgument) {
            case 'sponsorName': this.EPOptions[object.columnName] = getEndPointOptionsForSponsor(); break;
            case 'unitName': this.EPOptions[object.columnName] = getEndPointOptionsForLeadUnit(); break;
            case 'fibiDepartment': this.EPOptions[object.columnName] = getEndPointOptionsForDepartment(); break;
            case 'fibiOrganization': this.EPOptions[object.columnName] = getEndPointOptionsForOrganization(); break;
            case 'fibiCountry': this.EPOptions[object.columnName] = getEndPointOptionsForCountry(); break;
            case 'profitCenterName': this.EPOptions[object.columnName] = getEndPointOptionsForProfitCentre(); break;
            case 'grantCodeName': this.EPOptions[object.columnName] = getEndPointOptionsForGrandCode(); break;
            case 'costCenterName': this.EPOptions[object.columnName] = getEndPointOptionsForCostCentre(); break;
            case 'fundCenterName': this.EPOptions[object.columnName] = getEndPointOptionsForFundCentre(); break;
            case 'claimTemplateName': this.EPOptions[object.columnName] = getEndPointOptionsForMappedClaimTemplate(); break;
            default: break;
        }
        this.EPOptions[object.columnName].defaultValue = object.answers[0].description ? object.answers[0].description : null;
        this.EPOptions[object.columnName].contextField = object.defaultValue || this.EPOptions[object.columnName].contextField;
    }
    setSearchFilterValue(data, answer, list) {
        if (data) {
            switch (list.filterType) {
                case 'Autosuggest':
                    answer.value = data[this.searchObjectMapping[list.lookupArgument]] || null;
                    answer.description = data[list.defaultValue];
                    break;
                case 'Elastic Search':
                    answer.description = data[list.defaultValue];
                    answer.value = data[this.searchObjectMapping[list.lookupArgument]] || null;
                    break;
            }
        } else {
            answer.value = '';
            answer.description = '';
        }
        this.emitDataChange();
    }

    onLookupSelect(data, answer) {
        answer.value = data.length ? data[0].code : '';
        answer.description = data.length ? data[0].description : '';
        this.emitDataChange();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }
    /**
     * @param  {} customField
     * @param  {} event
     * @param  {} list
     * @param  {} id
     * check null,length and type validations on change
     */
    checkValidation(customField, event, list, id) {
        if (event.target.value.length < list.dataLength) {
            this.isLength = false;
        } else {
            this.lengthValidationId = id;
            this.isLength = true;
            customField.value = event.target.value = event.target.value.slice(0, list.dataLength);
        }
        if (list.filterType === 'Number' && event.keyCode >= 65 && event.keyCode <= 90) {
            this.isType = true;
            this.numberValidationId = id;
            customField.value = event.target.value.slice(0, 0);
        } else {
            this.isType = false;
        }
    }
    /**
     * check mandatory validation.
     *  data description and corresponding type codes are listed below.
     * 1-String, 2-Number, 3-Date, 4-Check Box, 5-Radio Button, 6-Elastic Search, 7-End Point Search, 8-System Lookup, 9-User Lookup
     */
    checkMandatory() {
        this.checkEmptyFlag = false;
        this.radioEmptyFlag = false;
        this.customElements.forEach((field, index) => {
            if (field.filterType !== 'Radio Button' && field.filterType !== 'Check Box') {
                const INDEX = field.answers.findIndex(item => (item.value === null || item.value === ''));
                if (INDEX >= 0 && field.isRequired === 'Y') {
                    this.isValueEmpty[index] = false;
                    this.validationId[index] = index;
                } else {
                    this.isValueEmpty[index] = true;
                }
            }
            this.checkEmptyFlag = false;
            this.radioEmptyFlag = false;
            if (field.filterType === 'Check Box' && field.isRequired === 'Y') {
                this.checkEmptyFlag = !!field.answers.find(item => item.value === true || item.value === 'true');
            }
            if (this.checkEmptyFlag === true) {
                this.checkEmpty[index] = false;
                this.validationId[index] = index;
            } else {
                this.checkEmpty[index] = true;
            }
            if (field.filterType === 'Radio Button' && field.isRequired === 'Y') {
                this.radioEmptyFlag = !!field.answers.find(item => item.value !== null && item.value !== '');
            }
            if (this.radioEmptyFlag === true) {
                this.radioEmpty[index] = false;
                this.validationId[index] = index;
            } else {
                this.radioEmpty[index] = true;
            }
        });
    }

    saveCustomData() {
        this.checkMandatory();
        if ((this.isValueEmpty.filter(item => item === false).length !== 0) ||
            (this.checkEmpty.filter(check => check === false).length !== 0) ||
            (this.radioEmpty.filter(radio => radio === false).length !== 0)) {
            this.isEmpty = true;
        } else {
            this.isEmpty = false;
        }
        if (this.isEmpty === false && this.checkEmptyFlag === false && this.radioEmptyFlag === false) {
            this.isLength = false;
            const CUSTOM_DATA: any = {};
            CUSTOM_DATA.updateTimestamp = new Date().getTime();
            CUSTOM_DATA.moduleItemKey = this.moduleItemKey;
            CUSTOM_DATA.moduleCode = this.moduleCode;
            CUSTOM_DATA.customElements = this.customElements;
            if (!this.isSaving) {
                this.isSaving = true;
                this.$subscriptions.push(this._customService.saveCustomData(CUSTOM_DATA)
                    .subscribe(data => {
                        this.result = data || [];
                        if (this.result !== null) {
                            if (this.isShowSave) {
                                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Other Information(s) saved successfully.');
                            }
                            this.customElements = this.result.customElements;
                            this.isRadioEmpty = true;
                            this.isDataChange = false;
                            this.dataChangeEvent.emit(false);
                        }
                        this.isSaving = false;
                    }, err => {
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Saving Other Information(s) failed. Please try again.');
                        this.isSaving = false;
                    }));
            }
        }
    }

    emitDataChange() {
        if (!this.isDataChange) {
            this.isDataChange = true;
            this.dataChangeEvent.emit(this.isDataChange);
        }
    }
}
