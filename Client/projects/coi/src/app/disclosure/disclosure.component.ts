import {Component, OnDestroy, OnInit} from '@angular/core';
import {ActivatedRoute, NavigationEnd, Router} from "@angular/router";
import {subscriptionHandler} from "../../../../fibi/src/app/common/utilities/subscription-handler";
import {Subscription} from "rxjs";
import {SfiService} from './sfi/sfi.service';
import {COI} from "./coi-interface";
import {DataStoreService} from "./services/data-store.service";
import {CoiService} from "./services/coi.service";
import {Location} from "@angular/common";

@Component({
    selector: 'app-disclosure',
    templateUrl: './disclosure.component.html',
    styleUrls: ['./disclosure.component.scss']
})
export class DisclosureComponent implements OnInit, OnDestroy {

    isCardExpanded = true;
    isCreateMode = false;
    isSaving = false;
    certificationText = 'I certify that the information provided for the Financial conflict of interest, including, responses to screening questions, list of my pertinent Significant Financial interests and possible relationship to my sponsored activity is an accurate and current statement of my reportable outside interests and activities.';
    $subscriptions: Subscription[] = [];
    coiData = new COI();
    currentStepNumber: 1 | 2 | 3 | 4 = 1;

    constructor(public router: Router,
                private _route: ActivatedRoute,
                public sfiService: SfiService,
                public coiService: CoiService,
                public location: Location,
                public dataStore: DataStoreService) {
        this.isCreateMode = this.router.url.includes('create-disclosure');
        this.setStepFirstTime(this.router.url);
        this.$subscriptions.push(this.router.events.subscribe(event => {
            if (event instanceof NavigationEnd) {
                this.isCreateMode = event.url.includes('create-disclosure');
            }
        }));
    }

    ngOnInit() {
        this.getDataFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    setStepFirstTime(currentUrl) {
        if (currentUrl.includes('create-disclosure/screening')) {
            this.currentStepNumber = 1;
        } else if (currentUrl.includes('create-disclosure/sfi')) {
            this.currentStepNumber = 2;
        } else if (currentUrl.includes('create-disclosure/relationship')) {
            this.currentStepNumber = 3;
        } else if (currentUrl.includes('create-disclosure/certification')) {
            this.currentStepNumber = 4;
        }
    }

    goToStep(stepPosition?: any) {
        if (!stepPosition && this.currentStepNumber == 4) {
            return;
        }
        this.currentStepNumber = stepPosition ? stepPosition : this.currentStepNumber + 1;
        this.navigateToStep();
    }

    goBackStep() {
        if (this.currentStepNumber == 1) {
            return;
        }
        this.currentStepNumber--;
        this.navigateToStep();
    }

    navigateToStep() {
        let nextStepUrl = '';
        switch (this.currentStepNumber) {
            case 1:
                nextStepUrl = '/coi/create-disclosure/screening';
                break;
            case 2:
                nextStepUrl = '/coi/create-disclosure/sfi';
                break;
            case 3:
                nextStepUrl = '/coi/create-disclosure/relationship';
                break;
            case 4:
                nextStepUrl = '/coi/create-disclosure/certification';
                break;
        }
        this.router.navigate([nextStepUrl], {queryParamsHandling: 'preserve'})
    }

    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'warning';
            case 2:
                return 'success';
            case 3:
                return 'danger';
            default:
                return 'info';
        }
    }

    getReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'warning';
            case '2':
                return 'info';
            case '3':
                return 'success';
            default:
                return 'danger';
        }
    }

    getDisclosureStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'warning';
            case '2':
            case '4':
            case '5':
                return 'info';
            case '3':
            case '6':
                return 'success';
            default:
                return 'danger';
        }
    }

    certifyDisclosure() {
        if (!this.isSaving && this.coiService.isCertified) {
            this.isSaving = true;
            const REQUESTREPORTDATA = {
                coiDisclosure: {
                    disclosureId: this.coiData.coiDisclosure.disclosureId,
                    certificationText: this.coiData.coiDisclosure.certificationText ? this.coiData.coiDisclosure.certificationText : this.certificationText
                }
            };
            this.$subscriptions.push(this.coiService.certifyDisclosure(REQUESTREPORTDATA).subscribe((res: any) => {
                this.dataStore.updateStore(['coiDisclosure'], {coiDisclosure: res});
                this.isSaving = false;
                this.router.navigate(['/coi/disclosure/summary'], {queryParamsHandling: 'preserve'});
            }));
        }
    }

    private getDataFromStore() {
        this.coiData = this.dataStore.getData();
        this._route.queryParams.subscribe(params => {
            const MODULE_ID = params['disclosureId'];
            if (!MODULE_ID) {
                this.router.navigate([], {
                    queryParams: {
                        disclosureId: this.coiData.coiDisclosure.disclosureId
                    },
                    queryParamsHandling: 'merge',
                });
            }
        });
    }
}
