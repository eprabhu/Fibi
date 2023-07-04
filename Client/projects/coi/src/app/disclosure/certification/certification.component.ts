import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import {DataStoreService} from "../../disclosure/services/data-store.service";
import {CoiService} from "../../disclosure/services/coi.service";
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { CommonService } from '../../common/services/common.service';

@Component({
    selector: 'app-certification',
    templateUrl: './certification.component.html',
    styleUrls: ['./certification.component.scss']
})
export class CertificationComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    certificationText = 'I agree to abide by the University COI policy guidelines and certify that the information provided for the Financial conflict of interest, including, responses to screening questions, list of my pertinent Significant Financial interests and possible relationship to my sponsored activity is an accurate and current statement of my reportable outside interests and activities.';
    dependencies = ['coiDisclosure'];
    isEditMode = true;
    isSaving = false;
    coiDisclosure: any;
    isReadMore = false;
    headerInfoText = `University policy requires that university officers, faculty, and staff and others acting on its
    behalf avoid ethical, legal, financial, and other conflicts of interest and ensure that their activities and
    interests do not conflict with their obligations to the University. Disclosure of financial interests enables
    the University to determine if a financial interest creates a conflict of interest or the appearance of a
    conflict of interest. The existence of a conflict or the appearance of one does not imply wrongdoing and
    does not necessarily mean that a researcher may not retain his or her financial interest and undertake the
    affected research. Often the University can work with the researcher to manage a conflict or the appearance
    of a conflict so that the research can continue in a way that minimizes the possibility of bias and preserves
    the objectivity of the research. Proper management depends on full and prompt disclosure. COI provides the ability
    to disclose and maintain your Significant Financial Interests; identify potential areas of concern related to your
     proposals and awards; and, disclose reimbursed travel (for NIH compliance).`;

    constructor(public _dataStore: DataStoreService, public _coiService: CoiService, public commonService: CommonService) { }

    ngOnInit() {
        this._coiService.isShowCertifyInfo = true;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }
    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
        this._coiService.isCertified = false;
    }

    private getDataFromStore() {
        const DATA = this._dataStore.getData(this.dependencies);
        this.coiDisclosure = DATA.coiDisclosure;
        if (this.coiDisclosure && this.coiDisclosure.certifiedBy) {
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
