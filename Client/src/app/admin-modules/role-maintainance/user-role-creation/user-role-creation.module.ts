import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { UserRoleCreationComponent } from './user-role-creation.component';
import { RouterModule } from '@angular/router';
import { SharedModule } from '../../../shared/shared.module';
import { FormsModule } from '@angular/forms';

@NgModule({
	imports: [
		CommonModule,
		RouterModule.forChild([{ path: '', component: UserRoleCreationComponent }]),
		SharedModule,
		FormsModule
	],
	declarations: [UserRoleCreationComponent]
})
export class UserRoleCreationModule { }
