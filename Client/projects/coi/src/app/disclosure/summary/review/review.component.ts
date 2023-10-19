import { Component, OnInit } from '@angular/core';
import { CoiSummaryEventsAndStoreService } from '../coi-summary-events-and-store.service';
import { Subscription } from 'rxjs';
import { DataStoreService } from '../../services/data-store.service';
import { CommonService } from '../../../common/services/common.service';
import { openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-coi-review',
    templateUrl: './review.component.html',
    styleUrls: ['./review.component.css']
})
export class ReviewComponent implements OnInit {

    isToolkitVisible = true;
    coiDetails: any = {};
    isRelationCollapsed = true;
    $subscriptions: Subscription[] = [];
    selectedProject: any = {};

    constructor(
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _dataStore: DataStoreService, public commonService: CommonService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.listenDataChangeFromStore();
    }

    fetchCOIDetails(): void {
        this.coiDetails = this._dataStoreAndEventsService.getData(
            this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId,
            ['coiDisclosure']
        ).coiDisclosure;
    }

    toggleToolkitVisibility(): void {
        this.isToolkitVisible = !this.isToolkitVisible;
        this._dataStoreAndEventsService.$isToolkitVisible.next(this.isToolkitVisible);
    }
    
    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.fetchCOIDetails();
            })
        );
    }
 
    openModuleSummaryDetails(event = null) {
        this.selectedProject = event;
        if (event) {
            openModal('projectViewModal');
        }
    }

    openProjectMoreDetails(moduleId) {
        let redirectUrl = '';
        if (this.selectedProject.moduleCode == 3) {
            redirectUrl = this.commonService.fibiApplicationUrl + '#/fibi/proposal/overview?proposalId=' + moduleId;
        } else {
            redirectUrl = this.commonService.fibiApplicationUrl + '#/fibi/award/overview?awardId=' + moduleId;
        }
        window.open(redirectUrl);
    }
    
}
