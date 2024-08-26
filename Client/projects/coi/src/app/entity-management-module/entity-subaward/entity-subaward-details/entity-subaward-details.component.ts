import { Component, Input, OnInit } from '@angular/core';

@Component({
    selector: 'app-entity-subaward-details',
    templateUrl: './entity-subaward-details.component.html',
    styleUrls: ['./entity-subaward-details.component.scss']
})
export class EntitySubawardDetailsComponent implements OnInit {

    @Input() sectionName: any;
    @Input() sectionId: any;
    notificationTemplates = [];
    templateOptions = 'EMPTY#EMPTY#false#false';
    defaultTemplateLabel = '';


    constructor() { }

    ngOnInit() {
    }

}
