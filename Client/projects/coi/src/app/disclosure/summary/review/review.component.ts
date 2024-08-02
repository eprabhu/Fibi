import { Component, OnInit } from '@angular/core';
import { CoiSummaryEventsAndStoreService } from '../coi-summary-events-and-store.service';
import { Subscription } from 'rxjs';
import { DataStoreService } from '../../services/data-store.service';
import { CommonService } from '../../../common/services/common.service';
import { openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { CoiService } from '../../services/coi.service';
import { CoiSummaryService } from '../coi-summary.service';
import { ProjectRelationshipDetails } from '../../coi-interface';

@Component({
    selector: 'app-coi-review',
    templateUrl: './review.component.html',
    styleUrls: ['./review.component.scss']
})
export class ReviewComponent implements OnInit {

    isToolkitVisible = true;
    coiDetails: any = {};
    isRelationCollapsed = true;
    $subscriptions: Subscription[] = [];
    selectedProject: ProjectRelationshipDetails;

    constructor(
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _dataStore: DataStoreService, public commonService: CommonService, public coiService: CoiService,
        public coiSummaryService: CoiSummaryService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.listenDataChangeFromStore();
        this.listenToolKitFocusSection();
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
 
    openModuleSummaryDetails(event: any) {
        if (event) {
            this.commonService.openProjectDetailsModal(event, null);
        }
    }

    // 'COI803' parent element of Relationships header.
    private listenToolKitFocusSection() {
        this.$subscriptions.push(this.coiService.$isExpandSection.subscribe(ele =>{
            if (ele.section == 'COI803') {
                this.isRelationCollapsed = ele.isExpand;
            }
        }));
    }

    clearHighlightOnRelationshipCollapse() {
        this.isRelationCollapsed = !this.isRelationCollapsed;
        this.coiSummaryService.activeSubNavItemId = '';
    }

}
