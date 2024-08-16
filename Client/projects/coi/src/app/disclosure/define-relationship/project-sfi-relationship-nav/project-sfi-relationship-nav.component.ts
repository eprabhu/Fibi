import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { CoiService } from '../../services/coi.service';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';

@Component({
    selector: 'app-project-sfi-relationship-nav',
    templateUrl: './project-sfi-relationship-nav.component.html',
    styleUrls: ['./project-sfi-relationship-nav.component.scss'],
    animations: []
})
export class ProjectSfiRelationshipNavComponent implements OnInit, OnDestroy {

    @Input() projectDetails: any = {};
    @Input() offsetHeight = 0;
    @Input() activeCounter = 0;
    @Input() projects: any[] = [];

    IS_SFI_PROJECT_SWITCH = true;
    searchText = '';
    currentProject: any;
    currentTab: 'PROJECT' | 'SFI' = 'PROJECT';
    $subscriptions: Subscription[] = [];
    conflictStatuses: any[] = [
        { label: 'No Conflict', count: 0, color: 'text-success' },
        { label: 'Potential Conflict', count: 0, color: 'text-warning' },
        { label: 'Conflict Identified', count: 0, color: 'text-danger' }
    ];

    constructor(public coiService: CoiService) { }
    
    ngOnInit(): void { }


    getFilteredListForSearchWord(): void {

    }

    resetList(): void {
        this.searchText = '';
    }

    switchTab(tab: 'PROJECT' | 'SFI'): void {
        this.currentTab = tab;
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}


