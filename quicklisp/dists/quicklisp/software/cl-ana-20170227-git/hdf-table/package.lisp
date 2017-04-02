;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013, 2014 Gary Hollis
;;;; 
;;;; This file is part of cl-ana.
;;;; 
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(defpackage #:cl-ana.hdf-table 
  (:use #:cl
	#:cffi
        #:alexandria
        #:cl-ana.list-utils
	#:cl-ana.hdf-utils
	#:cl-ana.hdf-cffi
	#:cl-ana.table
	#:cl-ana.typed-table
	#:cl-ana.typespec
	#:cl-ana.hdf-typespec
	#:cl-ana.string-utils
	#:cl-ana.functional-utils
        #:cl-ana.binary-tree
        #:cl-ana.memoization)
  (:export :hdf-table
           :hdf-table-nrows
	   :open-hdf-table
	   :create-hdf-table
	   :hdf-table-chain
           :hdf-table-chain-nrows
	   :open-hdf-table-chain))
