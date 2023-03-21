import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {EntityManagementComponent} from './entity-management.component';
import {RouterModule, Routes} from "@angular/router";

const routes: Routes = [{path: '', component: EntityManagementComponent}];

@NgModule({
    declarations: [
        EntityManagementComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes)
    ]
})
export class EntityManagementModule {
}
