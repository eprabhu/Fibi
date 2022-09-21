import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';

import { HTTP_SUCCESS_STATUS, HTTP_ERROR_STATUS } from '../../../../app-constants';
import { ExternalReviewerExt } from '../../reviewer-maintenance.interface';
import { ExtReviewerMaintenanceService } from '../../external-reviewer-maintenance-service';
import { subscriptionHandler } from '../../../../common/utilities/subscription-handler';
import { CommonService } from '../../../../common/services/common.service';
import { getEndPointOptionsForExtReviewerKeyWords } from '../../../../common/services/end-point.config';

@Component({
    selector: 'app-additional-details',
    templateUrl: './additional-details.component.html',
    styleUrls: ['./additional-details.component.css']
})
export class AdditionalDetailsComponent implements OnInit, OnDestroy {
    $subscriptions: Subscription[] = [];
    map = new Map();
    lookUpData: any;
    keywordWaringMsg = '';
    externalReviewerSpecializations = [];
    extReviewerId: number | string;
    hIndex: any = [];
    externalReviewerExt: ExternalReviewerExt = new ExternalReviewerExt();
    externalReviewerDetails: any = {};
    keywordHttpOptions: any = {};
    countrySearchHttpOptions: any;
    isSaving = false;
    clearKeywordField: String;


    constructor(public _extReviewerMaintenanceService: ExtReviewerMaintenanceService,
        private _commonService: CommonService) { }

    ngOnInit() {
        this.setInitialValues();
        this.getExternalReviewerData();
        this.makeDropdown();
    }

    getExternalReviewerData(): void {
        this.$subscriptions.push(this._extReviewerMaintenanceService.$externalReviewerDetails.subscribe((data: any) => {
            if (data.extReviewer) {
                this.externalReviewerDetails = JSON.parse(JSON.stringify(data));
                if (data.externalReviewerExt) {
                    this.externalReviewerExt = this.externalReviewerDetails.externalReviewerExt;
                }
                if (data.externalReviewerSpecializations) {
                    this.externalReviewerSpecializations = this.externalReviewerDetails.externalReviewerSpecializations;
                }
            }
        }));
    }

    setInitialValues() {
        this.lookUpData = this._extReviewerMaintenanceService.lookUpData;
        this.keywordHttpOptions = getEndPointOptionsForExtReviewerKeyWords();
    }

    setFunctionOptions(data) {
        return {
            arrayList: data.extReviewerSpecialization,
            contextField: 'description',
            filterFields: 'description',
            formatString: 'description',
            defaultValue: ''
        };
    }

    makeDropdown() {
        for (let i = 1; i <= 150; i++) {
            this.hIndex.push(i);
        }
    }

    // keywords section
    keywordChangeHandler(data) {
        if (data) {
            this.keywordWaringMsg = '';
            if (data !== null && data !== 'null') {
                if (this.checkDuplicateKeywords(data.description) !== undefined) {
                    this.keywordWaringMsg = 'Keyword already exist in database';
                    setTimeout(() => this.keywordWaringMsg = '', 4000);
                    this.keywordHttpOptions = getEndPointOptionsForExtReviewerKeyWords();

                } else {
                    this.saveKeyword(data);
                }
            }
        }
    }

    saveKeyword(data) {
        let isFoundInDataBase = false;
        this.externalReviewerSpecializations.map(item => {
            if (item.extReviewerSpecialization.description === data.description) {
                if (item.extReviewerSpecializationId) {
                    item.actionType = null;
                } else {
                    item.actionType = 'I';
                }
                isFoundInDataBase = true;
            }
        });
        if (!isFoundInDataBase) {
            this.externalReviewerSpecializations.push({
                externalReviewerId: this.externalReviewerDetails.extReviewerId,
                actionType: 'I',
                specializationCode: data.code,
                extReviewerSpecialization: data,
            });
        }
        this._extReviewerMaintenanceService.isDataChange = true;
        this.keywordHttpOptions = getEndPointOptionsForExtReviewerKeyWords();

    }

    checkDuplicateKeywords(keyword) {
        return this.externalReviewerSpecializations.find(item =>
            item.extReviewerSpecialization.description === keyword && (item.actionType == 'I' || item.actionType == null));
    }

    deleteFromKeyword(index: number): void {
        this.externalReviewerSpecializations[index].actionType = 'D';
        this.keywordWaringMsg = '';
        this._extReviewerMaintenanceService.isDataChange = true;
    }

    setCIRACode() {
        this.externalReviewerExt.extReviewerCira = this.lookUpData.extReviewerCira.find(cira =>
            cira.ciraCode === this.externalReviewerExt.ciraCode);
        this._extReviewerMaintenanceService.isDataChange = true;
    }

    setOriginality() {
        this.externalReviewerExt.extReviewerOriginality = this.lookUpData.extReviewerOriginality.find(originality =>
            originality.orginalityCode === this.externalReviewerExt.orginalityCode);
        this._extReviewerMaintenanceService.isDataChange = true;
    }

    setThoroughness() {
        this.externalReviewerExt.extReviewerThoroughness = this.lookUpData.extReviewerThoroughness.find(througness =>
            througness.thoroughnessCode === this.externalReviewerExt.thoroughnessCode);
        this._extReviewerMaintenanceService.isDataChange = true;
    }

    additionalDetailsValidation() {
        this.externalReviewerExt.scopusUrl = this.externalReviewerExt.scopusUrl.trim();
        this.map.clear();
        if (this.externalReviewerExt.hIndex == null && this.externalReviewerExt.hIndex != 0) {
            this.map.set('hIndex', 'hIndex');
        }
        if (!this.externalReviewerExt.scopusUrl) {
            this.map.set('scopusProfile', 'scopusProfile');
        }
    }

    setRequestObject() {
        const REQUESTREPORTDATA: any = {};
        this.externalReviewerExt.externalReviewerId = this.externalReviewerDetails.extReviewerId;
        if (this.externalReviewerExt.disciplinaryField) {
            this.externalReviewerExt.disciplinaryField = this.externalReviewerExt.disciplinaryField.trim();
        }
        REQUESTREPORTDATA.externalReviewerExt = this.externalReviewerExt;
        if (this.externalReviewerSpecializations.length > 0) {
            REQUESTREPORTDATA.externalReviewerSpecializations = this.externalReviewerSpecializations;
        }
        return REQUESTREPORTDATA;
    }

    saveResponseData(data) {
        this.externalReviewerDetails.externalReviewerExt = data.externalReviewerExt;
        if (data.externalReviewerSpecializations) {
            this.externalReviewerDetails.externalReviewerSpecializations = data.externalReviewerSpecializations;
        }
        this._extReviewerMaintenanceService.setExternalReviewerDetails(this.externalReviewerDetails);
        this._extReviewerMaintenanceService.isDataChange = false;
    }

    saveAdditionalDetails() {
        if (!this.isSaving) {
            this.additionalDetailsValidation();
            if (this.map.size < 1) {
                this.isSaving = true;
                const REQUESTREPORTDATA = this.setRequestObject();
                this.$subscriptions.push(this._extReviewerMaintenanceService.saveOrUpdateAdditionalDetails(REQUESTREPORTDATA)
                    .subscribe((data: any) => {
                        if (data.externalReviewerExt.externalReviewerExtId) {
                            this.saveResponseData(data);
                            this._commonService.showToast(HTTP_SUCCESS_STATUS, data.message);
                            this.isSaving = false;
                        }
                    },
                        err => {
                            this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating additional details. Please try again.');
                            this.isSaving = false;
                        }
                    ));
            }
        }

    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

}
