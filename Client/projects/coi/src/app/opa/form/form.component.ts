import {AfterViewInit, Component, OnInit} from '@angular/core';
import {Subject, Subscription} from 'rxjs';
import {FBConfiguration, FormBuilderEvent} from '../../shared/form-builder-view/form-builder-interface';
import { OpaService } from '../services/opa.service';
import {DataStoreService} from '../services/data-store.service';
import {OPA} from "../opa-interface";

@Component({
    selector: 'app-form',
    templateUrl: './form.component.html',
    styleUrls: ['./form.component.scss']
})
export class FormComponent implements OnInit, AfterViewInit {

    formBuilderEvents = new Subject<FormBuilderEvent>();
    fbConfiguration = new FBConfiguration();
    isFormEditMode = this.dataStore.getEditModeForOPA();
    opa: OPA = new OPA();
    $subscriptions: Subscription[] = [];

    constructor(public _opa: OpaService, private dataStore: DataStoreService) {
    }

    ngOnInit() {
        this.getDataFromStore();
        this.listenToDataChange();
    }

    ngAfterViewInit(): void {
        // NEEDS TO SETUP FORM BUILDER DATA HERE, currently adding dummy data fro save testing
        this.fbConfiguration.moduleItemCode = '23';
        this.fbConfiguration.moduleSubItemCode = '0';
        this.fbConfiguration.moduleItemKey = this.opa.opaDisclosure.opaDisclosureId.toString();
        this.fbConfiguration.moduleSubItemKey = '0';
        this.fbConfiguration.documentOwnerPersonId = this.opa.opaDisclosure.personId;
        this.fbConfiguration.formBuilderId = this.opa.opaDisclosure.opaFormBuilderDetails[0].formBuilderId;
        this._opa.formBuilderEvents.next({eventType: 'CONFIGURATION', data: this.fbConfiguration});
        this.updateFormEditMode();
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
        const latestIsFormEditMode = this.dataStore.getEditModeForOPA();
        this._opa.formBuilderEvents.next({eventType: 'IS_EDIT_MODE', data: latestIsFormEditMode});
        this.isFormEditMode = latestIsFormEditMode;
    }

    getDataFromStore() {
        this.opa = this.dataStore.getData();
    }

}
