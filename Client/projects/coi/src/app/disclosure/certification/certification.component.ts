import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import {DataStoreService} from "../services/data-store.service";
import {CoiService} from "../services/coi.service";
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";

@Component({
    selector: 'app-certification',
    templateUrl: './certification.component.html',
    styleUrls: ['./certification.component.scss']
})
export class CertificationComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    certificationText = 'I certify that the information provided for the Financial conflict of interest, including, responses to screening questions, list of my pertinent Significant Financial interests and possible relationship to my sponsored activity is an accurate and current statement of my reportable outside interests and activities.';
    dependencies = ['coiDisclosure'];
    isEditMode = true;
    isSaving = false;
    coiDisclosure: any;

    constructor(private _dataStore: DataStoreService, public _coiService: CoiService) { }

    ngOnInit() {
        this._coiService.isShowCertifyInfo = true;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }
    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore() {
        const DATA = this._dataStore.getData(this.dependencies);
        this.coiDisclosure = DATA.coiDisclosure;
        if (this.coiDisclosure.certifiedBy) {
            this.isEditMode = false;
            this._coiService.isCertified = true;
        }
        // this.isEditMode = this._dataStore.getEditModeForCOI();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dependencies.includes(dep))) {
                    this.getDataFromStore();
                }
            })
        );
    }

    certifyDisclosure() {
        if (!this.isSaving) {
            this.isSaving = true;
            const REQUESTREPORTDATA = {
                coiDisclosure: {
                    disclosureId: this.coiDisclosure.disclosureId,
                    certificationText: this.coiDisclosure.certificationText ? this.coiDisclosure.certificationText : this.certificationText
                }
            };
            this.$subscriptions.push(this._coiService.certifyDisclosure(REQUESTREPORTDATA).subscribe((res: any) => {
                this._dataStore.updateStore(['coiDisclosure'], { coiDisclosure: res });
                this.isSaving = false;
            }));
        }
    }

    closeCertifyInfo() {
        this._coiService.isShowCertifyInfo = false;
    }

}
