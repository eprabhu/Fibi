import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {EntityManagementComponent} from './entity-management.component';
import {RouterModule, Routes} from "@angular/router";
import { EntityListComponent } from './entity-list/entity-list.component';
import { ViewEntityDetailsComponent } from './view-entity-details/view-entity-details.component';
import { MatIconModule } from '@angular/material/icon';

const routes: Routes = [
  { path: '', component: EntityManagementComponent }
];

@NgModule({
    declarations: [
        EntityManagementComponent,
        EntityListComponent,
        ViewEntityDetailsComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule
    ],
    exports:[
    ]
})
export class EntityManagementModule {
}
