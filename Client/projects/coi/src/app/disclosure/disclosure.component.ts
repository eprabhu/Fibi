import {Component, OnDestroy, OnInit} from '@angular/core';
import {ActivatedRoute, NavigationEnd, Router} from "@angular/router";
import {subscriptionHandler} from "../../../../fibi/src/app/common/utilities/subscription-handler";
import {Subscription} from "rxjs";
import {SfiService} from './sfi/sfi.service';
import {COI} from "./coi-interface";
import {DataStoreService} from "./services/data-store.service";

@Component({
    selector: 'app-disclosure',
    templateUrl: './disclosure.component.html',
    styleUrls: ['./disclosure.component.scss']
})
export class DisclosureComponent implements OnInit, OnDestroy {

    isCardExpanded = true;
    isCreateMode = false;
    $subscriptions: Subscription[] = [];
    coiData = new COI();

    constructor(public _router: Router, private _route: ActivatedRoute,  public sfiService: SfiService,
                private _dataStore: DataStoreService) {
        this.isCreateMode = this._router.url.includes('create-disclosure');
        this.$subscriptions.push(this._router.events.subscribe(event => {
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

    private getDataFromStore() {
        this.coiData = this._dataStore.getData();
        this._route.queryParams.subscribe(params => {
            const MODULE_ID = params['disclosureId'];
            if (!MODULE_ID) {
                this._router.navigate([], {
                    queryParams: {
                        disclosureId: this.coiData.coiDisclosure.disclosureId
                    },
                    queryParamsHandling: 'merge',
                });
            }
        });
    }

    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case 2: return 'success';
            case 3: return 'danger';
            default: return 'info';
        }
    }

    getReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2': return 'info';
            case '3': return 'success';
            default: return 'danger';
        }
    }

    getDisclosureStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2':
            case '4':
            case '5':
                return 'info';
            case '3': case '6': return 'success';
            default: return 'danger';
        }
    }
}
