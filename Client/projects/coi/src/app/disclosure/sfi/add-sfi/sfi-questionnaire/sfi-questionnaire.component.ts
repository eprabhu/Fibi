import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, Observable, Subscription } from 'rxjs';

import { CoiFinancialEntity, CoiFinancialEntityDetail } from '../add-sfi.interface';
import { AddSfiService } from '../add-sfi.service';
import {subscriptionHandler} from "../../../../../../../fibi/src/app/common/utilities/subscription-handler";
import {CommonService} from "../../../../common/services/common.service";

declare var $: any;

@Component({
    selector: 'app-sfi-questionnaire',
    templateUrl: './sfi-questionnaire.component.html',
    styleUrls: ['./sfi-questionnaire.component.css']
})
export class SfiQuestionnaireComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    $externalSaveEvent = new BehaviorSubject<Boolean>(null);

    configuration: any = {
        moduleItemCode: 8,
        moduleSubitemCodes: [801],
        moduleItemKey: '',
        moduleSubItemKey: '',
        actionUserId: this._commonService.getCurrentUserDetail('personID'),
        actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
        enableViewMode: false,
        isChangeWarning: true,
        isEnableVersion: true,
    };
    relationLookup: any = [];
    definedRelationships: any = [];
    activeRelationship: any = 0;
    coiFinancialEntityDetail: CoiFinancialEntityDetail = new CoiFinancialEntityDetail();
    isEditMode = false;
    isAddRelationButtonToggled = false;
    entityDetails: CoiFinancialEntity = new CoiFinancialEntity();
    relationValidationMap = new Map();

    constructor(
        private _commonService: CommonService,
        private _addSFIService: AddSfiService,
        private _activatedRoute: ActivatedRoute,
        private _router: Router
    ) { }

    ngOnInit() {
        this.getDataFromService();
        this.isEditMode = this._activatedRoute.snapshot.queryParamMap.get('mode') === 'edit';
        this.configuration.enableViewMode = !this.isEditMode;
    }

    private getDataFromService() {
        this.entityDetails = JSON.parse(JSON.stringify(this._addSFIService.sfiDetails.coiFinancialEntity));
        this.relationLookup = JSON.parse(JSON.stringify(this._addSFIService.lookups.coiFinancialEntityRelType));
        this.definedRelationships = JSON.parse(JSON.stringify(this._addSFIService.sfiDetails.coiFinancialEntityDetails));
        this.configuration.moduleItemKey = this._addSFIService.sfiDetails.coiFinancialEntity.coiFinancialEntityId;
        this.coiFinancialEntityDetail.coiFinancialEntityId = this._addSFIService.sfiDetails.coiFinancialEntity.coiFinancialEntityId;
        if (this.definedRelationships.length > 0) {
            this.getQuestionnaire(this.definedRelationships[0]);
        }
        this.removeExistingRelation();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private removeExistingRelation() {
        if (this.definedRelationships.length) {
            this.activeRelationship = this.definedRelationships[0].financialEntityRelTypeCode;
            this.definedRelationships.forEach(element => {
                this.findRelation(element.financialEntityRelTypeCode);
            });
        }
    }

    private findRelation(financialEntityRelTypeCode: string) {
        const RELATION_INDEX = this.relationLookup.findIndex(element => element.financialEntityRelTypeCode === financialEntityRelTypeCode);
        if (RELATION_INDEX !== -1) {
            this.relationLookup.splice(RELATION_INDEX, 1);
        }
    }

    getQuestionnaire(data: any) {
        this.activeRelationship = data.financialEntityRelTypeCode;
        this.configuration.moduleSubItemKey = data.financialEntityRelTypeCode;
        this.configuration = Object.assign({}, this.configuration);
    }

    setRelationship() {
        this.coiFinancialEntityDetail.coiFinancialEntityRelType = this.relationLookup.find(
            ele => ele.financialEntityRelTypeCode == this.coiFinancialEntityDetail.financialEntityRelTypeCode
        );
    }

    saveOrUpdateCoiFinancialEntityDetails() {
        if (this.validateRelationship()) {
            this.$subscriptions.push(this._addSFIService.saveOrUpdateCoiFinancialEntityDetails({
                coiFinancialEntityDetail: this.coiFinancialEntityDetail
            }).subscribe((data: any) => {
                this.definedRelationships.push(data);
                this.getQuestionnaire(data.coiFinancialEntityRelType);
                this.findRelation(data.financialEntityRelTypeCode);
                this.clearRelationModal();
                // this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Financial Entity Relations saved successfully.');
            }, _err => {
                // this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving financial entity relations. Please try again.');
            }));
        }
    }

    addRelations(flag = false) {
        $('#relation-selection').modal('show');
        this.isAddRelationButtonToggled = flag;
    }

    clearRelationModal() {
        this.relationValidationMap.clear();
        this.coiFinancialEntityDetail.coiFinancialEntityRelType = null;
        this.coiFinancialEntityDetail.financialEntityRelTypeCode = null;
        $('#relation-selection').modal('hide');
    }

    getSaveEvent(_event) {
        this.relationLookup.length ? this.addRelations() : this.navigateBack();
        this.$externalSaveEvent.next(true);
    }

    navigateBack() {
        this._router.navigateByUrl(this._addSFIService.previousURL);
    }

    getRelationDescription() {
        switch (this.activeRelationship) {
            case '1': return `your`;
            case '2': return `spouse's`;
            case '3': return `Dependant's`;
            default: return '';
        }
    }

    validateRelationship() {
        this.relationValidationMap.clear();
        if (!this.coiFinancialEntityDetail.financialEntityRelTypeCode) {
            this.relationValidationMap.set('relationRadio', 'Please select a relation to continue.');
        }
        return this.relationValidationMap.size === 0 ? true : false;
    }

    // isShowProceedButton() {
    //     return this._addSFIService.previousURL.includes('coi/');
    // }

}
