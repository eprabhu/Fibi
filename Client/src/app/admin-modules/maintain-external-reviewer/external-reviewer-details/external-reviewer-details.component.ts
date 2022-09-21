import { Component, OnInit, OnDestroy } from '@angular/core';
import { ExtReviewerMaintenanceService } from '../external-reviewer-maintenance-service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { ExtReviewer } from '../reviewer-maintenance.interface';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonService } from '../../../common/services/common.service';
declare var $: any;
@Component({
    selector: 'app-external-reviewer-details',
    templateUrl: './external-reviewer-details.component.html',
    styleUrls: ['./external-reviewer-details.component.css']
})

export class ExternalReviewerDetailsComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    extReviewer: ExtReviewer = {};
    mode: string;
    isMaintainReviewer = false;
    lookUpData: any;


    constructor(
        public _extReviewerMaintenanceService: ExtReviewerMaintenanceService,
        private _activatedRoute: ActivatedRoute,
        private _router: Router,
        private _commonService: CommonService
    ) { }

    async ngOnInit() {
        this.isMaintainReviewer = await this._extReviewerMaintenanceService.getMaintainReviewerPermission();
        this.setInitialValues();
        this.getExternalReviewerData();
        this.$subscriptions.push(this._activatedRoute.queryParams.subscribe(params => {
            if (params.mode) {
                this._extReviewerMaintenanceService.mode = params.mode;
            }
        }));
        this._extReviewerMaintenanceService.navigationUrl = this._router.url.split('?')[0];
    }

    setInitialValues() {
        this.lookUpData = this._extReviewerMaintenanceService.lookUpData;
    }

    getExternalReviewerData(): void {
        this.$subscriptions.push(this._extReviewerMaintenanceService.$externalReviewerDetails.subscribe((data: any) => {
            if (data.extReviewer) {
                this.extReviewer = data.extReviewer;
            }
        }));
    }

    closeSaveAndExitModal() {
        $('#saveAndExitModal').modal('hide');
        this._extReviewerMaintenanceService.isDataChange = false;
        this._extReviewerMaintenanceService.navigationUrl ? this.redirectToTabs() : this.redirectToFibiDashboard();
    }

    redirectToTabs() {
        this._router.navigate([this._extReviewerMaintenanceService.navigationUrl], { queryParamsHandling: 'merge' });
        this._extReviewerMaintenanceService.navigationUrl = '';
    }

    redirectToFibiDashboard() {
        this._router.navigate([this._commonService.dashboardNavigationURL]);
        this._commonService.dashboardNavigationURL = '';
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

}
