import {AfterViewInit, Component, OnInit} from '@angular/core';
import {Subject, Subscription} from 'rxjs';
import {FBConfiguration, FormBuilderEvent} from '../../shared/form-builder-view/form-builder-interface';
import { OpaService } from '../services/opa.service';
import {DataStoreService} from '../services/data-store.service';
import {OPA} from "../opa-interface";
import { coiReviewComment } from '../../shared-components/shared-interface';
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { ActivatedRoute } from '@angular/router';

@Component({
    selector: 'app-form',
    templateUrl: './form.component.html',
    styleUrls: ['./form.component.scss']
})
export class FormComponent implements OnInit, AfterViewInit {

    formBuilderEvents = new Subject<FormBuilderEvent>();
    fbConfiguration = new FBConfiguration();
    isFormEditMode = this.dataStore.isFormEditable();
    opa: OPA = new OPA();
    $subscriptions: Subscription[] = [];
    formBuilderId: any;

    constructor(public _opa: OpaService, private dataStore: DataStoreService, private _commonService: CommonService, private _route: ActivatedRoute) {
    }

    ngOnInit() {
        this.getDataFromStore();
        this.listenToDataChange();
        window.scrollTo(0,0);
        this._route.queryParamMap.subscribe(queryParams => {
            this.formBuilderId = queryParams.get('formBuilderId');
            if (this.formBuilderId) {
            }
        });
    }

    ngAfterViewInit(): void {
        if(this.formBuilderId){
            this.fbConfiguration.moduleItemCode = '23';
            this.fbConfiguration.moduleSubItemCode = '0';
            this.fbConfiguration.documentOwnerPersonId = this.opa.opaDisclosure.personId;
            this.fbConfiguration.formBuilderId = this.formBuilderId;
            this._opa.formBuilderEvents.next({eventType: 'BLANK_FORM', data: this.fbConfiguration});
        }else{
            // NEEDS TO SETUP FORM BUILDER DATA HERE, currently adding dummy data fro save testing
            this.fbConfiguration.moduleItemCode = '23';
            this.fbConfiguration.moduleSubItemCode = '0';
            this.fbConfiguration.moduleItemKey = this.opa.opaDisclosure.opaDisclosureId.toString();
            this.fbConfiguration.moduleSubItemKey = '0';
            this.fbConfiguration.documentOwnerPersonId = this.opa.opaDisclosure.personId;
            this.fbConfiguration.formBuilderId = 206;
            this._opa.formBuilderEvents.next({eventType: 'BLANK_FORM', data: this.fbConfiguration});
            this.updateFormEditMode();
        }
    }

    triggerSave() {
        this._opa.formBuilderEvents.next({eventType: 'SAVE'});
    }

    private listenToDataChange() {
        this.$subscriptions.push(this.dataStore.dataEvent.subscribe((res) => {
            this.getDataFromStore();
            this.updateFormEditMode();
        }));
    }

    updateFormEditMode() {
        const latestIsFormEditMode = this.dataStore.isFormEditable();
        this._opa.formBuilderEvents.next({eventType: 'IS_EDIT_MODE', data: latestIsFormEditMode});
        this.isFormEditMode = latestIsFormEditMode;
    }

    getDataFromStore() {
        this.opa = this.dataStore.getData();
    }

    commentSliderEvent(event) {
        const COMMENT_META_DATA: any = {
            documentOwnerPersonId: this.opa.opaDisclosure.personId,
            componentTypeCode: event.componentTypeCode,
            formBuilderComponentId: event.formBuilderComponentId,
            formBuilderId : event.formBuilderId,
            formBuilderSectionId : event.formBuilderSectionId,
            headerName: event.headerName
        }
        this._commonService.$commentConfigurationDetails.next(COMMENT_META_DATA);
        this._opa.isShowCommentNavBar = true;
    }

    formBuilderDataChanged(formEvent: any) {
        switch (formEvent) {
            case 'CHANGED':
                return this._opa.isFormBuilderDataChangePresent = true;
            case 'SAVE_COMPLETE': {
                if(this._opa.isFormBuilderDataChangePresent) {
                    this._opa.triggerSaveComplete.next(true);
                }
                return this._opa.isFormBuilderDataChangePresent = false;
            }
            case 'NEW_SFI':
                this.loadOPA();
        }
    }

    loadOPA() {
        this.$subscriptions.push(this._opa.loadOPA(this.opa.opaDisclosure.opaDisclosureId).subscribe((data) => {
            this.dataStore.updateStore(['opaDisclosure'], {opaDisclosure: data});
        }));
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }
}
